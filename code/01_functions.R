# helpers from 1_df_preparation.R ------------------------------------------

add_missing_coords <- function(df, coords_df) {
  df |>
    left_join(
      coords_df |> select(id, decimal_latitude, decimal_longitude),
      by = "id"
    ) |>
    mutate(
      decimal_latitude = coalesce(decimal_latitude.x, decimal_latitude.y),
      decimal_longitude = coalesce(decimal_longitude.x, decimal_longitude.y)
    ) |>
    select(-ends_with(".x"), -ends_with(".y"))
}

label_bg_sample <- function(sf_obj, label) {
  sf_obj |>
    mutate(
      source = label,
      institution_code = label,
      availability = label,
      data_type = label
    )
}

# Split Species Names ----------------------------------------------------

split_verbatim_scientific_name <- function(
  df,
  column = "verbatimScientificName"
) {
  df %>%
    mutate(
      match = str_match(
        .[[column]],
        "^([A-Z][a-z]+)(?:\\s([a-z]+))?(?:\\s*\\(?([A-Za-z\\.\\s]+),\\s*(\\d{4})\\)?)?"
      ),
      v_genus = match[, 2],
      v_species = match[, 3],
      v_authority_name = match[, 4],
      v_authority_date = match[, 5]
    ) %>%
    select(-match)
}


# nnpca (adapted from ggpca) ---------------------------------------------

nnpca <- function(
  data,
  metadata_cols,
  mode = c("pca", "tsne", "umap"),
  scale = TRUE,
  x_pc = "PC1",
  y_pc = "PC2",
  color_var = NULL,
  ellipse = TRUE,
  ellipse_level = 0.9,
  ellipse_type = "norm",
  ellipse_alpha = 0.9,
  point_size = 3,
  point_alpha = 0.6,
  facet_var = NULL,
  tsne_perplexity = 30,
  umap_n_neighbors = 15,
  density_plot = "none",
  color_palette = "Set1",
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  filter = NULL,
  bg_filter = NULL,
  bg_colour = "grey80",
  bg_alpha = 0.3,
  bg_size = 1.5,
  #loadings controls
  loadings = FALSE,
  loadings_multiplier = 2,
  loadings_arrow_multiplier = 3,
  loadings_arrow_length = 0.2, #in cm
  loadings_color = "black",
  loadings_label_size = 3,
  loadings_label_offset = 1.12 #push labels beyond arrow tips (multiplier)
) {
  mode <- match.arg(mode)
  density_plot <- match.arg(density_plot, choices = c("x", "y", "both", "none"))
  if (is.numeric(metadata_cols)) {
    metadata <- data[, metadata_cols, drop = FALSE]
    features <- data[, -metadata_cols, drop = FALSE]
  } else if (is.character(metadata_cols)) {
    metadata <- data[, metadata_cols, drop = FALSE]
    features <- data[, !names(data) %in% metadata_cols, drop = FALSE]
  } else {
    stop(
      "metadata_cols should be either a numeric vector or a character vector."
    )
  }
  features <- dplyr::select_if(features, is.numeric)
  explained_variance <- NULL
  if (mode == "pca") {
    pca <- stats::prcomp(features, scale. = scale)
    scores <- as.data.frame(pca$x)
    colnames(scores) <- paste0("PC", seq_len(ncol(scores)))
    explained_variance <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    xlab <- if (is.null(xlab)) {
      paste0(x_pc, " (", explained_variance[1], "% variance)")
    } else {
      xlab
    }
    ylab <- if (is.null(ylab)) {
      paste0(y_pc, " (", explained_variance[2], "% variance)")
    } else {
      ylab
    }

    #  prepare loadings dataframe when requested
    loadings_df <- NULL
    if (isTRUE(loadings)) {
      rot <- as.data.frame(pca$rotation)
      rot2 <- rot[, seq_len(min(2, ncol(rot))), drop = FALSE]
      colnames(rot2) <- c("PC1", "PC2")
      loadings_df <- as.data.frame(rot2)
      loadings_df$variable <- rownames(rot2)
      loadings_df$PC1_scaled <- loadings_df$PC1 * loadings_multiplier
      loadings_df$PC2_scaled <- loadings_df$PC2 * loadings_multiplier
      loadings_df$xend <- loadings_df$PC1_scaled * loadings_arrow_multiplier
      loadings_df$yend <- loadings_df$PC2_scaled * loadings_arrow_multiplier
      # compute label positions slightly beyond arrow tips
      loadings_df$label_x <- loadings_df$xend * loadings_label_offset
      loadings_df$label_y <- loadings_df$yend * loadings_label_offset
    }
    # end loadings prep
  } else if (mode == "tsne") {
    tsne_result <- Rtsne::Rtsne(
      as.matrix(features),
      perplexity = tsne_perplexity,
      check_duplicates = FALSE
    )
    scores <- as.data.frame(tsne_result$Y)
    colnames(scores) <- c("Dim1", "Dim2")
    x_pc <- "Dim1"
    y_pc <- "Dim2"
    xlab <- if (is.null(xlab)) {
      x_pc
    } else {
      xlab
    }
    ylab <- if (is.null(ylab)) {
      y_pc
    } else {
      ylab
    }
  } else if (mode == "umap") {
    umap_result <- umap::umap(
      as.matrix(features),
      n_neighbors = umap_n_neighbors
    )
    scores <- as.data.frame(umap_result$layout)
    colnames(scores) <- c("UMAP1", "UMAP2")
    x_pc <- "UMAP1"
    y_pc <- "UMAP2"
    xlab <- if (is.null(xlab)) {
      x_pc
    } else {
      xlab
    }
    ylab <- if (is.null(ylab)) {
      y_pc
    } else {
      ylab
    }
  }
  plot_data <- dplyr::bind_cols(metadata, scores)

  # Validate filter/bg_filter variables exist in metadata to avoid confusing 'match' errors
  extract_vars_from_expr <- function(expr_input) {
    if (is.null(expr_input)) {
      return(character(0))
    }
    if (is.character(expr_input)) {
      expr_parsed <- rlang::parse_expr(expr_input)
    } else {
      expr_parsed <- expr_input
    }
    if (rlang::is_formula(expr_parsed)) {
      expr_parsed <- rlang::f_rhs(expr_parsed)
    }
    # ensure we have a language object for all.vars
    expr_lang <- expr_parsed
    all.vars(expr_lang)
  }

  plot_colnames <- colnames(plot_data)

  # check bg_filter
  if (!is.null(bg_filter)) {
    bg_vars <- extract_vars_from_expr(bg_filter)
    missing_bg <- setdiff(bg_vars, plot_colnames)
    if (length(missing_bg) > 0) {
      stop(
        "`bg_filter` references missing columns in metadata: ",
        paste(missing_bg, collapse = ", ")
      )
    }
  }

  # check main filter
  if (!is.null(filter)) {
    filter_vars <- extract_vars_from_expr(filter)
    missing_filter <- setdiff(filter_vars, plot_colnames)
    if (length(missing_filter) > 0) {
      stop(
        "`filter` references missing columns in metadata: ",
        paste(missing_filter, collapse = ", ")
      )
    }
  }

  # background data filter
  plot_data_full <- plot_data
  bg_data <- NULL
  if (!is.null(bg_filter)) {
    if (is.character(bg_filter)) {
      expr_bg <- rlang::parse_expr(bg_filter)
      bg_data <- dplyr::filter(plot_data_full, !!expr_bg)
    } else if (rlang::is_formula(bg_filter)) {
      expr_bg <- rlang::f_rhs(bg_filter)
      bg_data <- dplyr::filter(plot_data_full, !!expr_bg)
    } else {
      fq_bg <- rlang::enquo(bg_filter)
      if (!rlang::quo_is_null(fq_bg)) {
        bg_data <- dplyr::filter(plot_data_full, !!fq_bg)
      }
    }
    if (!is.null(bg_data) && nrow(bg_data) == 0) bg_data <- NULL
  }
  # Apply main filter to plot_data
  if (!is.null(filter)) {
    if (is.character(filter)) {
      expr <- rlang::parse_expr(filter)
      plot_data <- dplyr::filter(plot_data, !!expr)
    } else if (rlang::is_formula(filter)) {
      expr <- rlang::f_rhs(filter)
      plot_data <- dplyr::filter(plot_data, !!expr)
    } else {
      fq <- rlang::enquo(filter)
      if (!rlang::quo_is_null(fq)) {
        plot_data <- dplyr::filter(plot_data, !!fq)
      }
    }
    if (nrow(plot_data) == 0) {
      stop("`filter` removed all rows; nothing to plot.")
    }
  }

  if (!is.null(color_var)) {
    if (!color_var %in% colnames(plot_data)) {
      stop("`color_var` not found in the provided data. Check column names.")
    }
    aes_params <- ggplot2::aes(
      x = .data[[x_pc]],
      y = .data[[y_pc]],
      color = .data[[color_var]]
    )
  } else {
    aes_params <- ggplot2::aes(x = .data[[x_pc]], y = .data[[y_pc]])
  }
  p <- ggplot2::ggplot(plot_data, aes_params)

  if (!is.null(bg_data)) {
    p <- p +
      ggplot2::geom_point(
        data = bg_data,
        ggplot2::aes(x = !!rlang::sym(x_pc), y = !!rlang::sym(y_pc)),
        colour = bg_colour,
        alpha = bg_alpha,
        size = bg_size,
        inherit.aes = FALSE
      )
  }

  p <- p +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(t = 5),
        size = 10
      ),
      axis.title.y = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(t = 5),
        size = 10
      ),
      axis.text.x = ggplot2::element_text(
        size = 8,
        face = "bold",
        color = "black"
      ),
      axis.text.y = ggplot2::element_text(
        size = 8,
        face = "bold",
        color = "black"
      )
    )

  if (!is.null(color_var)) {
    if (is.numeric(plot_data[[color_var]])) {
      p <- p + ggplot2::scale_color_gradient(low = "blue", high = "red")
    } else {
      p <- p +
        if (length(color_palette) > 1) {
          ggplot2::scale_color_manual(values = color_palette)
        } else {
          ggplot2::scale_color_brewer(palette = color_palette)
        }
    }
  }
  if (ellipse && mode == "pca") {
    if (!is.null(color_var) && !is.numeric(plot_data[[color_var]])) {
      p <- p +
        ggplot2::stat_ellipse(
          ggplot2::aes(group = .data[[color_var]]),
          level = ellipse_level,
          type = ellipse_type,
          alpha = ellipse_alpha
        )
    } else if (is.null(color_var)) {
      p <- p +
        ggplot2::stat_ellipse(
          level = ellipse_level,
          type = ellipse_type,
          alpha = ellipse_alpha
        )
    } else {
      message(
        "Ellipses are only supported for discrete color variables. Skipping ellipses."
      )
    }
  }
  if (!is.null(facet_var)) {
    if (inherits(facet_var, "formula")) {
      p <- p + ggplot2::facet_grid(facet_var)
    } else {
      stop("facet_var should be a formula (e.g., x ~ y, x ~ ., or . ~ y).")
    }
  }

  # add loadings arrows + labels BEFORE any cowplot/ggdraw conversion
  if (isTRUE(loadings) && mode == "pca" && !is.null(loadings_df)) {
    p <- p +
      ggplot2::geom_segment(
        data = loadings_df,
        ggplot2::aes(x = 0, y = 0, xend = xend, yend = yend),
        arrow = grid::arrow(length = grid::unit(loadings_arrow_length, "cm")),
        colour = loadings_color,
        inherit.aes = FALSE
      ) +
      ggrepel::geom_text_repel(
        data = loadings_df,
        # use label_x/label_y so text sits beyond the arrow tip
        ggplot2::aes(x = label_x, y = label_y, label = variable),
        colour = loadings_color,
        size = loadings_label_size,
        inherit.aes = FALSE,
        segment.size = 0.3
      )
  }
  # end added loadings layer

  fill_mapping <- if (!is.null(color_var)) {
    ggplot2::aes(fill = .data[[color_var]])
  } else {
    ggplot2::aes()
  }
  if (density_plot %in% c("x", "both")) {
    xdens <- ggplot2::ggplot(plot_data, fill_mapping) +
      ggplot2::geom_density(alpha = 0.7, linewidth = 0.2) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      {
        if (!is.null(color_var) && is.numeric(plot_data[[color_var]])) {
          ggplot2::scale_fill_gradient(low = "blue", high = "red")
        } else if (!is.null(color_var)) {
          if (length(color_palette) > 1) {
            ggplot2::scale_fill_manual(values = color_palette)
          } else {
            ggplot2::scale_fill_brewer(palette = color_palette)
          }
        } else {
          ggplot2::scale_fill_manual(values = c("gray50"))
        }
      } +
      ggplot2::aes(x = .data[[x_pc]])
    p <- cowplot::insert_xaxis_grob(
      p,
      xdens,
      grid::unit(0.2, "null"),
      position = "top"
    )
  }
  if (density_plot %in% c("y", "both")) {
    ydens <- ggplot2::ggplot(plot_data, fill_mapping) +
      ggplot2::geom_density(alpha = 0.7, linewidth = 0.2) +
      ggplot2::theme_classic() +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none") +
      {
        if (!is.null(color_var) && is.numeric(plot_data[[color_var]])) {
          ggplot2::scale_fill_gradient(low = "blue", high = "red")
        } else if (!is.null(color_var)) {
          if (length(color_palette) > 1) {
            ggplot2::scale_fill_manual(values = color_palette)
          } else {
            ggplot2::scale_fill_brewer(palette = color_palette)
          }
        } else {
          ggplot2::scale_fill_manual(values = c("gray50"))
        }
      } +
      ggplot2::aes(x = .data[[y_pc]])
    p <- cowplot::insert_yaxis_grob(
      p,
      ydens,
      grid::unit(0.2, "null"),
      position = "right"
    )
  }
  if (density_plot != "none") {
    p <- cowplot::ggdraw(p)
  }

  return(p)
}


# ── Ecological-district map helpers (unified from scripts 3 & 4) ──────────────

#' Minimal map theme
#' @param base_size  base font size
#' @param title_face font face for plot title ("bold" for genus, "italic" for species)
#' @param legend_key_h legend key height (cm)
#' @param legend_key_w legend key width (cm)
#' @param legend_text_size legend text size (pt)
#' @param legend_title_size legend title size (pt)
theme_map <- function(
  base_size = 10,
  title_face = "italic",
  subtitle_offset = 1.5,
  legend_key_h = 0.55,
  legend_key_w = 0.28,
  legend_text_size = 7,
  legend_title_size = 8
) {
  theme_void(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = title_face,
        size = base_size,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = base_size - subtitle_offset,
        colour = "grey30",
        hjust = 0.5
      ),
      legend.key.height = unit(legend_key_h, "cm"),
      legend.key.width = unit(legend_key_w, "cm"),
      legend.text = element_text(size = legend_text_size),
      legend.title = element_text(size = legend_title_size)
    )
}

#' Minimal inset map theme
theme_inset <- function() {
  theme_void() +
    theme(
      panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.4)
    )
}

#' Integer-only scale breaks
breaks_integer <- function(n = 5) {
  function(limits) {
    max_val <- floor(limits[2])
    if (max_val <= 1) {
      return(1)
    }
    unique(round(pretty(c(1, max_val), n = min(n, max_val))))
  }
}

#' Viridis-plasma fill scale for record counts
scale_fill_records <- function(limits = NULL, show_legend = TRUE) {
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey92",
    name = "Records",
    breaks = breaks_integer(),
    labels = label_number(accuracy = 1),
    limits = limits,
    trans = "sqrt",
    guide = if (show_legend) "colourbar" else "none"
  )
}

#' Viridis-mako fill scale for difference (non-GBIF contribution)
scale_fill_diff <- function(limits = NULL, show_legend = TRUE) {
  scale_fill_viridis_c(
    option = "mako",
    na.value = "grey92",
    name = "Added\nrecords",
    breaks = breaks_integer(),
    labels = label_number(accuracy = 1),
    limits = limits,
    trans = "sqrt",
    direction = -1,
    guide = if (show_legend) "colourbar" else "none"
  )
}

#' Join counts to ecological-region sf layer
join_eco <- function(sf_layer, counts) {
  sf_layer |> left_join(counts, by = "ECOLOGICAL_REGION")
}

#' Add a Three Kings inset to a main map
#' @param inset_right right edge of inset (0.20 for genus, 0.18 for species)
add_inset <- function(
  main_plot,
  map_data,
  fill_col,
  scale,
  inset_right = 0.18
) {
  tk_data <- map_data |> filter(ECOLOGICAL_REGION == "Three Kings")

  inset <- ggplot(tk_data) +
    geom_sf(aes(fill = {{ fill_col }}), colour = "grey60", linewidth = 0.3) +
    scale +
    theme_inset() +
    theme(legend.position = "none")

  main_plot +
    inset_element(inset, left = 0, bottom = 0.75, right = inset_right, top = 1)
}

#' Plot a heatmap of record counts on ecological regions
#' @param title       plot title (e.g. species name or "Anagotus")
#' @param counts      data frame with ECOLOGICAL_REGION + count columns
#' @param count_col   unquoted column name for fill
#' @param subtitle    subtitle string
#' @param scale       a ggplot2 fill scale (e.g. scale_fill_records())
#' @param linewidth   border linewidth for regions
#' @param map_theme   theme function call (e.g. theme_map(title_face = "bold"))
#' @param inset_right right edge of Three Kings inset
plot_heatmap <- function(
  title,
  counts,
  count_col,
  subtitle,
  scale,
  sf_eco,
  linewidth = 0.15,
  map_theme = theme_map(),
  inset_right = 0.18
) {
  map_data <- join_eco(sf_eco, counts)

  p <- ggplot(map_data) +
    geom_sf(
      aes(fill = {{ count_col }}),
      colour = "grey60",
      linewidth = linewidth
    ) +
    scale +
    labs(title = title, subtitle = subtitle) +
    map_theme

  add_inset(p, map_data, {{ count_col }}, scale, inset_right = inset_right)
}

#' Plot difference (non-GBIF contribution) on ecological regions
#' @param title       plot title
#' @param counts      data frame with n_diff, n_gbif columns
#' @param scale       fill scale (e.g. scale_fill_diff())
#' @param linewidth   border linewidth
#' @param map_theme   theme function call
#' @param inset_right right edge of Three Kings inset
#' @param subtitle    optional subtitle (NULL to omit)
plot_diff <- function(
  title,
  counts,
  scale,
  sf_eco,
  linewidth = 0.15,
  map_theme = theme_map(),
  inset_right = 0.18,
  subtitle = NULL
) {
  counts_aug <- counts |>
    mutate(
      diff_plot = if_else(n_diff > 0, as.numeric(n_diff), NA_real_),
      has_gbif_only = n_gbif > 0 & n_diff == 0
    )

  map_data <- join_eco(sf_eco, counts_aug)

  p <- ggplot(map_data) +
    geom_sf(fill = "grey92", colour = "grey60", linewidth = linewidth) +
    geom_sf(
      data = \(d) filter(d, has_gbif_only),
      fill = "grey70",
      colour = "grey60",
      linewidth = linewidth
    ) +
    geom_sf(
      data = \(d) filter(d, !is.na(diff_plot)),
      aes(fill = diff_plot),
      colour = "grey60",
      linewidth = linewidth
    ) +
    scale +
    labs(title = title, subtitle = subtitle) +
    map_theme

  add_inset(p, map_data, diff_plot, scale, inset_right = inset_right)
}
