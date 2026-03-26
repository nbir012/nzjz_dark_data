source(here::here("code", "00_setup.R"))

# PCA for comparison --------------------------------------------------

pca_for_all <- df_pca_for_all %>%
  dplyr::select(
    -institution_code,
    -catalog_number,
    -location,
    -source,
    -specific_name,
    -availability,
    -data_type,
    -decimal_latitude,
    -decimal_longitude
  ) %>%
  prcomp(scale = TRUE)

# df_pca_for_all %>%
#   dplyr::select(
#     -availability,
#     -institution_code,
#     -source,
#     -species,
#     -data_type
#   ) %>%
#   prcomp(scale = TRUE) %>%
#   dist()

numeric_data <- df_pca_for_all |>
  dplyr::select(
    -institution_code,
    -catalog_number,
    -location,
    -source,
    -specific_name,
    -availability,
    -data_type,
    -decimal_latitude,
    -decimal_longitude
  )

pca_for_all <- prcomp(numeric_data, scale = TRUE)

pca_for_all_ind <- broom::augment(pca_for_all, data = numeric_data)

pca_scores <- as.data.frame(pca_for_all$x)

pca_with_meta <- dplyr::bind_cols(
  pca_scores,
  df_pca_for_all |>
    dplyr::select(
      availability,
      institution_code,
      source,
      specific_name,
      data_type
    )
)

# PCA on availability ------------------------------------------------------

df_fig_availability_pca <- df_pca_for_all %>%
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

fig_availability_pca <- nnpca(
  data = df_fig_availability_pca,
  metadata_cols = c(1:9),
  mode = "pca",
  color_var = "Availability",
  color_palette = "Set1",
  ellipse = TRUE,
  density_plot = "both",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  filter = "!`source` %in% c('sf_nz_random_sample_10k', 'sf_doc_random_sample_10k')",
  bg_filter = "`Availability` == 'sf_nz_random_sample_10k'", # background selection (same forms as `filter`)
  bg_colour = "grey80", # background point colour
  bg_alpha = 0.3, # background alpha
  bg_size = 1.5 # background point size
)

fig_availability_pca

ggsave(
  filename = here::here("output", "figures", "fig_availability_pca.png"),
  plot = fig_availability_pca,
  width = 8,
  height = 6,
  dpi = 300
)
