source(here::here("code", "00_setup.R"))

# ── 1. Load & dissolve ecological regions ─────────────────────────────────────

sf_eco <- file.path(
  dirname(gpkg_file),
  "Ecological_Districts_-2969373808597425294.gpkg"
) |>
  sf::st_read() |>
  group_by(ECOLOGICAL_REGION) |>
  summarise(.groups = "drop")

# ── 2. Occurrence counts per region x source (genus level) ────────────────────
# All records used — no species_keep filter

counts_genus <- sf_anagotus_combined |>
  select(generic_name, availability) |>
  sf::st_join(select(sf_eco, ECOLOGICAL_REGION), join = sf::st_within) |>
  sf::st_drop_geometry() |>
  filter(availability %in% c("GBIF", "non-GBIF")) |>
  count(ECOLOGICAL_REGION, availability) |>
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

# ── 3. Shared theme & scales ───────────────────────────────────────────────────

theme_map <- function(base_size = 10) {
  theme_void(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size, hjust = 0.5),
      plot.subtitle = element_text(
        size = base_size - 2,
        colour = "grey30",
        hjust = 0.5
      ),
      legend.key.height = unit(0.6, "cm"),
      legend.key.width = unit(0.3, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    )
}

theme_inset <- function() {
  theme_void() +
    theme(
      panel.border = element_rect(colour = "grey40", fill = NA, linewidth = 0.4)
    )
}

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

# ── 4. Geometry helpers ────────────────────────────────────────────────────────

join_eco <- function(sf_layer, counts) {
  sf_layer |> left_join(counts, by = "ECOLOGICAL_REGION")
}

add_inset <- function(main_plot, map_data, fill_col, scale) {
  tk_data <- map_data |> filter(ECOLOGICAL_REGION == "Three Kings")

  inset <- ggplot(tk_data) +
    geom_sf(aes(fill = {{ fill_col }}), colour = "grey60", linewidth = 0.3) +
    scale +
    theme_inset() +
    theme(legend.position = "none")

  main_plot +
    inset_element(inset, left = 0, bottom = 0.75, right = 0.2, top = 1)
}

# ── 5. Plot functions ──────────────────────────────────────────────────────────

plot_heatmap_genus <- function(counts, count_col, subtitle, scale) {
  map_data <- join_eco(sf_eco, counts)

  p <- map_data |>
    ggplot() +
    geom_sf(aes(fill = {{ count_col }}), colour = "grey60", linewidth = 0.2) +
    scale +
    labs(title = "Anagotus", subtitle = subtitle) +
    theme_map()

  add_inset(p, map_data, {{ count_col }}, scale)
}

plot_diff_genus <- function(counts) {
  counts_aug <- counts |>
    mutate(
      diff_plot = if_else(n_diff > 0, as.numeric(n_diff), NA_real_),
      has_gbif_only = n_gbif > 0 & n_diff == 0
    )

  map_data <- join_eco(sf_eco, counts_aug)

  diff_max <- max(counts$n_diff, na.rm = TRUE)
  diff_max <- if (is.finite(diff_max)) diff_max else 1L

  p <- map_data |>
    ggplot() +
    geom_sf(fill = "grey92", colour = "grey60", linewidth = 0.2) +
    geom_sf(
      data = \(d) filter(d, has_gbif_only),
      fill = "grey70",
      colour = "grey60",
      linewidth = 0.2
    ) +
    geom_sf(
      data = \(d) filter(d, !is.na(diff_plot)),
      aes(fill = diff_plot),
      colour = "grey60",
      linewidth = 0.2
    ) +
    scale_fill_diff(limits = c(1, diff_max)) +
    labs(title = "Anagotus", subtitle = "Non-GBIF contribution") +
    theme_map()

  add_inset(p, map_data, diff_plot, scale_fill_diff(limits = c(1, diff_max)))
}

# ── 6. Build three panels & assemble ──────────────────────────────────────────

# Replace 0 with NA so zero-count regions use na.value = "grey92"
counts_genus_clean <- counts_genus |>
  mutate(across(c(n_gbif, n_all, n_diff), \(x) na_if(x, 0L)))

shared_max <- max(counts_genus_clean$n_all, na.rm = TRUE)

p_gbif <- plot_heatmap_genus(
  counts_genus_clean,
  n_gbif,
  "GBIF only",
  scale_fill_records(limits = c(1, shared_max), show_legend = TRUE)
)
p_all <- plot_heatmap_genus(
  counts_genus_clean,
  n_all,
  "GBIF + non-GBIF",
  scale_fill_records(limits = c(1, shared_max), show_legend = TRUE)
)
p_diff <- plot_diff_genus(counts_genus_clean)

genus_figure <- (p_gbif | p_all | p_diff)


genus_difference <- p_diff

# ── 7. Save ────────────────────────────────────────────────────────────────────

out_dir <- here::here("output", "figures", "heatmaps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(
  file.path(out_dir, "genus_figure.png"),
  genus_figure,
  width = 21,
  height = 9,
  dpi = 200,
  bg = "white"
)
