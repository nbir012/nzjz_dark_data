source(here::here("code", "00_setup.R"))

# summary stats ----------------------------------------------------------
# compute 10-year breaks for x axis (extract numeric year first)
min_year <- df_anagotus_combined %>%
  dplyr::pull(year) %>%
  lubridate::year() %>%
  min(na.rm = TRUE)

max_year <- df_anagotus_combined %>%
  dplyr::pull(year) %>%
  lubridate::year() %>%
  max(na.rm = TRUE)

year_start <- floor(min_year / 10) * 10

year_end <- ceiling(max_year / 10) * 10

year_breaks <- seq(year_start, year_end, by = 10)

# stacked observations by decade and institution
fig_observations_by_year <- df_anagotus_combined %>%
  dplyr::mutate(year_num = lubridate::year(year)) %>%
  dplyr::mutate(decade_start = floor(year_num / 10) * 10) %>%
  dplyr::mutate(decade_label = paste0(decade_start, "s")) %>%
  dplyr::group_by(decade_start, decade_label, institution_code) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::arrange(decade_start) %>%
  dplyr::mutate(
    decade_label = factor(
      decade_label,
      levels = unique(as.character(decade_label))
    ),
    is_na = is.na(decade_start)
  ) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = decade_label,
    y = n,
    fill = is_na
  )) +
  ggplot2::geom_col() +
  ggplot2::facet_wrap(ggplot2::vars(institution_code), scales = "free_y") +
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  ggplot2::scale_x_discrete(
    breaks = function(x) c(x[seq(1, length(x) - 1, by = 2)], "NAs")
  ) +
  ggplot2::scale_fill_manual(
    values = c(`TRUE` = "#E69F00"),
    name = NULL
  ) +
  ggplot2::labs(
    x = "Decade",
    y = "Number of Specimens Collected"
  ) +
  ggplot2::theme_classic(base_size = 20) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

fig_observations_by_year

ggsave(
  filename = here::here("output", "figures", "observations_by_year.png"),
  plot = fig_observations_by_year,
  width = 35,
  height = 16,
  units = "cm",
  dpi = 300
)


grand_total <- nrow(df_anagotus_combined)

tbl_avail <- df_anagotus_combined |>
  count(institution_code, availability) |>
  pivot_wider(
    names_from = availability,
    values_from = n,
    values_fill = 0L
  ) |>
  rename(n_gbif = GBIF, n_non_gbif = `non-GBIF`) |>
  mutate(
    n_total = n_gbif + n_non_gbif,
    gbif_pct = n_gbif / n_total,
    non_gbif_pct = n_non_gbif / n_total,
    total_pct = n_total / grand_total,
    gbif_label = paste0(
      n_gbif,
      " (",
      scales::label_percent(accuracy = 0.1)(gbif_pct),
      ")"
    ),
    non_gbif_label = paste0(
      n_non_gbif,
      " (",
      scales::label_percent(accuracy = 0.1)(non_gbif_pct),
      ")"
    ),
    total_label = paste0(
      n_total,
      " (",
      scales::label_percent(accuracy = 0.1)(total_pct),
      ")"
    )
  ) |>
  arrange(-n_total) |>
  select(institution_code, gbif_label, non_gbif_label, total_label)

# --- totals row ---
totals <- df_anagotus_combined |>
  count(availability) |>
  pivot_wider(names_from = availability, values_from = n, values_fill = 0L) |>
  rename(n_gbif = GBIF, n_non_gbif = `non-GBIF`) |>
  mutate(
    n_total = n_gbif + n_non_gbif,
    gbif_pct = n_gbif / n_total,
    non_gbif_pct = n_non_gbif / n_total,
    total_pct = n_total / grand_total
  ) |>
  transmute(
    institution_code = "Total",
    gbif_label = paste0(
      n_gbif,
      " (",
      scales::label_percent(accuracy = 0.1)(gbif_pct),
      ")"
    ),
    non_gbif_label = paste0(
      n_non_gbif,
      " (",
      scales::label_percent(accuracy = 0.1)(non_gbif_pct),
      ")"
    ),
    total_label = paste0(
      n_total,
      " (",
      scales::label_percent(accuracy = 0.1)(total_pct),
      ")"
    )
  )

tbl_final <- bind_rows(tbl_avail, totals)

tbl_final |>
  flextable::flextable() |>
  flextable::set_header_labels(
    institution_code = "Institution",
    gbif_label = "Available on GBIF\nn (%)",
    non_gbif_label = "Not available on GBIF\nn (%)",
    total_label = "Total records\nn (%)"
  ) |>
  flextable::align(align = "center", part = "all") |>
  flextable::align(j = 1, align = "left", part = "all") |>
  flextable::bold(i = nrow(tbl_final), part = "body") |>
  flextable::hline(i = nrow(tbl_final) - 1) |>
  flextable::set_table_properties(layout = "autofit") |>
  flextable::save_as_docx(
    path = here::here("output", "tables", "online_offline_specimens.docx")
  )

# Species-level summary
species_summary <- counts |>
  summarise(
    .by = specific_name,
    gbif_regions = sum(n_gbif > 0, na.rm = TRUE),
    all_regions = sum(n_all > 0, na.rm = TRUE)
  ) |>
  mutate(
    change_pct = if_else(
      gbif_regions == 0,
      NA_real_,
      ((all_regions - gbif_regions) / gbif_regions) * 100
    )
  ) |>
  arrange(specific_name) |>
  rename(
    `Species` = specific_name,
    `GBIF regions (n)` = gbif_regions,
    `GBIF + non-GBIF regions (n)` = all_regions,
    `Change (%)` = change_pct
  )

species_summary |>
  flextable::flextable() |>
  flextable::colformat_double(j = "Change (%)", digits = 1, na_str = "*") |>
  flextable::italic(j = "Species", italic = TRUE, part = "body") |>
  flextable::footnote(
    i = ~ is.na(`Change (%)`),
    j = "Change (%)",
    value = flextable::as_paragraph(
      "No GBIF records in any region — percentage change not applicable"
    ),
    ref_symbols = "*",
    part = "body"
  ) |>
  flextable::set_table_properties(layout = "autofit") |>
  flextable::save_as_docx(
    path = here::here("output", "tables", "region_coverage_species.docx")
  )

# Genus-level summary
genus_summary <- counts_genus |>
  summarise(
    gbif_regions = sum(n_gbif > 0, na.rm = TRUE),
    all_regions = sum(n_all > 0, na.rm = TRUE)
  ) |>
  mutate(
    Genus = "Anagotus",
    change_pct = ((all_regions - gbif_regions) / gbif_regions) * 100
  ) |>
  select(
    `Genus` = Genus,
    `GBIF regions (n)` = gbif_regions,
    `GBIF + non-GBIF regions (n)` = all_regions,
    `Change (%)` = change_pct
  )

genus_summary |>
  flextable::flextable() |>
  flextable::colformat_double(j = "Change (%)", digits = 1) |>
  flextable::italic(j = "Genus", italic = TRUE, part = "body") |>
  flextable::set_table_properties(layout = "autofit") |>
  flextable::save_as_docx(
    path = here::here("output", "tables", "region_coverage_genus.docx")
  )
