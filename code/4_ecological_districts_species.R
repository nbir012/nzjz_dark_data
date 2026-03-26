source(here::here("code", "00_setup.R"))

# ── 1. Load & dissolve ecological regions ─────────────────────────────────────

sf_eco <- file.path(
  dirname(gpkg_file),
  "Ecological_Districts_-2969373808597425294.gpkg"
) |>
  sf::st_read() |>
  group_by(ECOLOGICAL_REGION) |>
  summarise(.groups = "drop")

# ── 2. Occurrence counts per species x region x source ────────────────────────

counts <- sf_anagotus_combined |>
  filter(specific_name %in% species_keep) |>
  select(specific_name, availability) |>
  sf::st_join(select(sf_eco, ECOLOGICAL_REGION), join = sf::st_within) |>
  sf::st_drop_geometry() |>
  filter(availability %in% c("GBIF", "non-GBIF")) |>
  count(specific_name, ECOLOGICAL_REGION, availability) |>
  pivot_wider(
    names_from = availability,
    values_from = n,
    values_fill = 0L,
    names_prefix = "n_"
  ) |>
  rename_with(\(x) gsub("-", "_", x)) |>
  mutate(
    n_all = n_GBIF + n_non_GBIF,
    n_diff = n_non_GBIF
  ) |>
  rename(n_gbif = n_GBIF)

# ── 3. Nest counts by species ──────────────────────────────────────────────────

counts_nested <- counts |>
  nest(data = -specific_name)

# ── 4. Shared theme ────────────────────────────────────────────────────────────

theme_map <- function(base_size = 10) {
  theme_void(base_size = base_size) +
    theme(
      plot.title = element_text(face = "italic", size = base_size, hjust = 0.5),
      plot.subtitle = element_text(size = base_size - 1.5, colour = "grey30"),
      legend.key.height = unit(0.55, "cm"),
      legend.key.width = unit(0.28, "cm"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 8)
    )
}

theme_inset <- function() {
  theme_void() +
    theme(
      panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.4)
    )
}

# ── 5. Scale helpers ───────────────────────────────────────────────────────────

breaks_integer <- function(n = 5) {
  function(limits) {
    max_val <- floor(limits[2])
    if (max_val <= 1) {
      return(1)
    }
    unique(round(pretty(c(1, max_val), n = min(n, max_val))))
  }
}

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

# ── 6. Geometry helpers ────────────────────────────────────────────────────────

join_eco <- function(sf_layer, species_counts) {
  sf_layer |> left_join(species_counts, by = "ECOLOGICAL_REGION")
}

add_inset <- function(main_plot, map_data, fill_col, scale) {
  tk_data <- map_data |> filter(ECOLOGICAL_REGION == "Three Kings")

  inset <- ggplot(tk_data) +
    geom_sf(aes(fill = {{ fill_col }}), colour = "grey60", linewidth = 0.3) +
    scale +
    theme_inset() +
    theme(legend.position = "none")

  main_plot +
    inset_element(inset, left = 0, bottom = 0.75, right = 0.18, top = 1)
}

# ── 7. Plot functions ──────────────────────────────────────────────────────────

plot_heatmap <- function(
  species_name,
  species_counts,
  count_col,
  title_suffix,
  scale
) {
  map_data <- join_eco(sf_eco, species_counts)

  p <- ggplot(map_data) +
    geom_sf(aes(fill = {{ count_col }}), colour = "grey60", linewidth = 0.15) +
    scale +
    labs(title = species_name, subtitle = title_suffix) +
    theme_map()

  add_inset(p, map_data, {{ count_col }}, scale)
}

plot_diff <- function(species_name, species_counts, scale) {
  species_counts_aug <- species_counts |>
    mutate(
      diff_plot = if_else(n_diff > 0, as.numeric(n_diff), NA_real_),
      has_gbif_only = n_gbif > 0 & n_diff == 0
    )

  map_data <- join_eco(sf_eco, species_counts_aug)

  p <- ggplot(map_data) +
    geom_sf(fill = "grey92", colour = "grey60", linewidth = 0.15) +
    geom_sf(
      data = \(d) filter(d, has_gbif_only),
      fill = "grey70",
      colour = "grey60",
      linewidth = 0.15
    ) +
    geom_sf(
      data = \(d) filter(d, !is.na(diff_plot)),
      aes(fill = diff_plot),
      colour = "grey60",
      linewidth = 0.15
    ) +
    scale +
    labs(title = species_name) +
    theme_map()

  add_inset(p, map_data, diff_plot, scale)
}

# ── 8. Per-species row builder ────────────────────────────────────────────────

build_species_row <- function(
  species_name,
  species_counts,
  global_records_max,
  global_diff_max
) {
  # Replace 0 with NA so zero-count regions use na.value = "grey92"
  species_counts_clean <- species_counts |>
    mutate(across(c(n_gbif, n_all, n_diff), \(x) na_if(x, 0L)))

  p_gbif <- plot_heatmap(
    species_name,
    species_counts_clean,
    n_gbif,
    "GBIF only",
    scale_fill_records(limits = c(1, global_records_max), show_legend = FALSE)
  )
  p_all <- plot_heatmap(
    species_name,
    species_counts_clean,
    n_all,
    "GBIF + non-GBIF",
    scale_fill_records(limits = c(1, global_records_max), show_legend = TRUE)
  )
  p_diff <- plot_diff(
    species_name,
    species_counts_clean,
    scale_fill_diff(limits = c(1, global_diff_max), show_legend = TRUE)
  )

  wrap_plots(p_gbif, p_all, p_diff, ncol = 3)
}

# ── 9. Build rows, split into figures ─────────────────────────────────────────

n_per_fig <- 4L

# Compute global scale limits across all species
global_records_max <- max(counts$n_all, na.rm = TRUE)
global_diff_max <- max(counts$n_diff, na.rm = TRUE)

species_rows <- counts_nested |>
  mutate(
    fig_num = ((row_number() - 1L) %/% n_per_fig) + 1L,
    row_plot = map2(
      specific_name,
      data,
      \(sp, d) build_species_row(sp, d, global_records_max, global_diff_max)
    )
  )

caption_text <- paste(
  # "Left: GBIF only",
  # "Centre: GBIF + non-GBIF  (shared scale across all species)",
  # "Right: Non-GBIF contribution",
  # sep = "  |  "
)

annotate_fig <- function(fig, subtitle) {
  fig +
    plot_annotation(
      theme = theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank()
      )
    )
}

figures <- species_rows |>
  nest(rows = -fig_num) |>
  mutate(
    fig = map2(
      fig_num,
      rows,
      \(fn, r) {
        sp_start <- (fn - 1L) * n_per_fig + 1L
        sp_end <- min(fn * n_per_fig, nrow(counts_nested))
        r |>
          pull(row_plot) |>
          wrap_plots(ncol = 1) |>
          annotate_fig(glue("Species {sp_start}\u2013{sp_end}"))
      }
    )
  )

# ── 10. Save ───────────────────────────────────────────────────────────────────

out_dir <- here::here("output", "figures", "heatmaps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

figures |>
  pwalk(\(fig_num, rows, fig) {
    ggsave(
      file.path(out_dir, glue("combined_heatmaps_p{fig_num}.png")),
      fig,
      width = 8,
      height = 10,
      dpi = 300,
      bg = "white",
      limitsize = FALSE
    )
  })
