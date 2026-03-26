source(here::here("code", "00_setup.R"))

##To investigate: some islands are missing enviro data. Possibly due to coords being in or close to the ocean. Need to check why the coords are not precise.

##https://bjnnowak.netlify.app/2021/09/15/r-pca-with-tidyverse/

# ggpca plot with xy density ------------------------------------------------
df_fig_institution_pca <- df_pca_for_all %>%
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


fig_institution_pca <- nnpca(
  data = df_fig_institution_pca,
  metadata_cols = c(1:9),
  mode = "pca",
  color_var = "Institution",
  color_palette = set1_custom,
  ellipse = FALSE,
  density_plot = "both",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  filter = "!`Institution` %in% 'sf_nz_random_sample_10k'",
  bg_filter = "`Institution` == 'sf_nz_random_sample_10k'",
  bg_colour = "grey50",
  bg_alpha = 0.05,
  bg_size = 1.5
)

fig_institution_pca

ggsave(
  filename = here::here("output", "figures", "fig_institution_pca.png"),
  plot = fig_institution_pca,
  width = 8,
  height = 6,
  dpi = 300
)
