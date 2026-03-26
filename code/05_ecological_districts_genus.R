source(here::here("code", "00_setup.R"))

# ── 1. Load & dissolve ecological regions ─────────────────────────────────────

sf_eco <- file.path(
  here::here("data"),
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

# ── 3. Genus-specific theme overrides ──────────────────────────────────────────

genus_theme <- theme_map(
  title_face = "bold",
  subtitle_offset = 2,
  legend_key_h = 0.6,
  legend_key_w = 0.3,
  legend_text_size = 8,
  legend_title_size = 9
)

# ── 4. Build three panels & assemble ──────────────────────────────────────────

# Replace 0 with NA so zero-count regions use na.value = "grey92"
counts_genus_clean <- counts_genus |>
  mutate(across(c(n_gbif, n_all, n_diff), \(x) na_if(x, 0L)))

shared_max <- max(counts_genus_clean$n_all, na.rm = TRUE)

p_gbif <- plot_heatmap(
  title = "Anagotus",
  counts = counts_genus_clean,
  count_col = n_gbif,
  subtitle = "GBIF only",
  scale = scale_fill_records(limits = c(1, shared_max), show_legend = TRUE),
  sf_eco = sf_eco,
  linewidth = 0.2,
  map_theme = genus_theme,
  inset_right = 0.2
)

p_all <- plot_heatmap(
  title = "Anagotus",
  counts = counts_genus_clean,
  count_col = n_all,
  subtitle = "GBIF + non-GBIF",
  scale = scale_fill_records(limits = c(1, shared_max), show_legend = TRUE),
  sf_eco = sf_eco,
  linewidth = 0.2,
  map_theme = genus_theme,
  inset_right = 0.2
)

diff_max <- max(counts_genus_clean$n_diff, na.rm = TRUE)
diff_max <- if (is.finite(diff_max)) diff_max else 1L

p_diff <- plot_diff(
  title = "Anagotus",
  counts = counts_genus_clean,
  scale = scale_fill_diff(limits = c(1, diff_max)),
  sf_eco = sf_eco,
  linewidth = 0.2,
  map_theme = genus_theme,
  inset_right = 0.2,
  subtitle = "Non-GBIF contribution"
)

genus_figure <- (p_gbif | p_all | p_diff)

genus_difference <- p_diff

# ── 5. Save ────────────────────────────────────────────────────────────────────

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
