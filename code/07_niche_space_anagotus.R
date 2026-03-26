source(here::here("code", "00_setup.R"))

df_fig_anagotus_pca <- df_pca_for_all %>%
  rename(
    `Mean Annual Temp (°C)` = temp_meanAnn,
    `Min July Temp (°C)` = temp_minJuly,
    `October VPD (kPa)` = vpd_oct,
    `Annual Water Deficit (mm)` = waterBalance_annDeficit,
    `Rain:PET Ratio` = waterBalance_R2PET,
    `Data Type` = data_type,
    `Institution` = institution_code,
    `Availability` = availability
  )

## Test drive loadings:
fig_anagotus_pca <- nnpca(
  data = df_fig_anagotus_pca,
  metadata_cols = c(1:9),
  mode = "pca",
  color_var = NULL,
  color_palette = "Set1",
  ellipse = TRUE,
  density_plot = "both",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  filter = "!`source` %in% c('sf_nz_random_sample_10k', 'sf_doc_random_sample_10k')",
  bg_filter = "Availability == 'sf_nz_random_sample_10k'",
  bg_colour = "grey80",
  bg_alpha = 0.3,
  bg_size = 1.5,
  loadings = TRUE,
  loadings_multiplier = ,
  loadings_arrow_multiplier = 2.5,
  loadings_arrow_length = , # in cm
  loadings_color = "#0072B2",
  loadings_label_size = 3.5
)

fig_anagotus_pca

ggsave(
  filename = here::here("output", "figures", "fig_anagotus_pca.png"),
  plot = fig_anagotus_pca,
  width = 8,
  height = 6,
  dpi = 300
)
