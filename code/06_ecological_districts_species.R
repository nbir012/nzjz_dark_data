source(here::here("code", "00_setup.R"))

# ── 1. Load & dissolve ecological regions ─────────────────────────────────────

sf_eco <- file.path(
  here::here("data"),
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

# ── 4. Per-species row builder ────────────────────────────────────────────────

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
    title = species_name,
    counts = species_counts_clean,
    count_col = n_gbif,
    subtitle = "GBIF only",
    scale = scale_fill_records(
      limits = c(1, global_records_max),
      show_legend = FALSE
    ),
    sf_eco = sf_eco
  )
  p_all <- plot_heatmap(
    title = species_name,
    counts = species_counts_clean,
    count_col = n_all,
    subtitle = "GBIF + non-GBIF",
    scale = scale_fill_records(
      limits = c(1, global_records_max),
      show_legend = TRUE
    ),
    sf_eco = sf_eco
  )
  p_diff <- plot_diff(
    title = species_name,
    counts = species_counts_clean,
    scale = scale_fill_diff(limits = c(1, global_diff_max), show_legend = TRUE),
    sf_eco = sf_eco
  )

  wrap_plots(p_gbif, p_all, p_diff, ncol = 3)
}

# ── 5. Build rows, split into figures ─────────────────────────────────────────

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

# ── 6. Save ───────────────────────────────────────────────────────────────────

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
