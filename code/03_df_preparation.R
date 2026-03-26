# install git pkgs --------------------------------------------------------

# remotes::install_git("git@github.com:nbir012/anagotus.data.git") # if needed

# libraries ---------------------------------------------------------------

source(here::here("code", "00_setup.R"))

# get data ----------------------------------------------------------------

df_anagotus <- anagotus.data::df_anagotus |>
  mutate(
    source = "anagotus.data",
    institution_code = case_when(
      is.na(institution_code) ~ NA_character_,
      institution_code == "NMMZ" ~ "MONZ",
      institution_code == "NMNZ" ~ "MONZ",
      institution_code == "WELT" ~ "CMNZ",
      institution_code == "AMNZ" ~ "AK",
      TRUE ~ institution_code
    )
  )

uk_gbif <- name_backbone_checklist("Anagotus") |>
  pull(usageKey)

## send download query to gbif (unhash if needed)
# occ_download(
#   pred_in("taxonKey", uk_gbif),
#   format = "DWCA"
# )
# occ_download_wait(1234567) # replace with your download id

df_gbif_dl <- occ_download_get("0025977-251009101135966") |>
  occ_download_import()

df_gbif <- df_gbif_dl |>
  filter(
    institutionCode != "iNaturalist",
    occurrenceStatus != "ABSENT",
    countryCode == "NZ"
  ) |>
  mutate(
    institutionCode = if_else(
      institutionCode %in% c("MWLR", "New Zealand Arthropod Collection"),
      "NZAC",
      institutionCode
    )
  ) |>
  split_verbatim_scientific_name() |>
  select(
    institution_code = institutionCode,
    catalog_number = catalogNumber,
    recorded_by = recordedBy,
    year,
    month,
    decimal_latitude = decimalLatitude,
    decimal_longitude = decimalLongitude,
    location = locality,
    verbatimLocality,
    generic_name = v_genus,
    specific_name = v_species,
    scientificName,
    verbatimScientificName
  ) |>
  unite(location, location:verbatimLocality, sep = "", remove = TRUE) |>
  unite("species", generic_name, specific_name, sep = " ", remove = FALSE) |>
  select(-verbatimScientificName, -scientificName) |>
  mutate(
    id = row_number(),
    source = "gbif",
    data_origin = case_when(
      is.na(institution_code) ~ "gbif_digitised",
      TRUE ~ paste0("gbif_", tolower(institution_code), "_digitised")
    )
  )


# select records with no coordinates but with locality info
df_missing_coords <- df_gbif |>
  filter(
    is.na(decimal_latitude),
    is.na(decimal_longitude),
    !is.na(location),
    location != ""
  )

## Manually added coordinates to missing locations; saved as .rds:
# write_rds(df_added_coords, here::here("data", "df_added_coords"))

df_added_coords <- read_rds(here::here("data", "df_added_coords"))

## QA check with arsenal (uncomment to verify join):
# df_gbif_join_test <- add_missing_coords(df_gbif, df_added_coords)
# comparison <- arsenal::comparedf(df_gbif, df_gbif_join_test, by = "id")
# summary(comparison)

# combine the added coords to gbif ---------------------------------------

df_gbif <- add_missing_coords(df_gbif, df_added_coords)


# Join df_gbif and df_anagotus -------------------------------------------

# --- Lookup table: pattern to canonical name ---
anagotus_lut <- c(
  # Starred n.sp. provisional names
  "Anagotus *asper n.sp." = "Anagotus species_a",
  "Anagotus *asper n.sp. [just n.sp.]" = "Anagotus species_a",
  "Anagotus *australis" = "Anagotus species_b",
  "Anagotus *chionochloae" = "Anagotus species_c",
  "Anagotus *heraldicus n.sp." = "Anagotus species_d",
  "Anagotus *cottieri" = "Anagotus species_e",
  "Anagotus *montivagus" = "Anagotus species_f",
  "Anagotus *graminicola n.sp." = "Anagotus species_g",
  "Anagotus *phyllocladi n.sp." = "Anagotus species_h",
  "Anagotus *porrectus" = "Anagotus species_i",
  "Anagotus *prolixus" = "Anagotus species_j",
  "Anagotus *triregius n.sp." = "Anagotus species_k",
  # Spelling corrections
  "Anagotus fairburnii" = "Anagotus fairburni",
  "Anagotus helmis" = "Anagotus helmsi",
  "Anagotus laevicostataus" = "Anagotus laevicostatus"
)

df_anagotus_combined <- bind_rows(df_anagotus, df_gbif) |>
  mutate(data_origin = str_to_lower(data_origin)) |>
  filter(!is.na(decimal_latitude), !is.na(decimal_longitude)) |> # filter before distinct to avoid losing observations
  filter(generic_name == "Anagotus") |>
  mutate(
    species = str_squish(species),
    species = case_when(
      # Fixed-string lookups (handles starred names + spelling fixes)
      str_to_lower(species) %in% str_to_lower(names(anagotus_lut)) ~
        anagotus_lut[match(
          str_to_lower(species),
          str_to_lower(names(anagotus_lut))
        )],
      # Indeterminate roll-ups
      str_detect(
        species,
        regex("^Anagotus\\s+(indet\\.|NA)$", ignore_case = TRUE)
      ) ~
        "Anagotus sp.",
      str_detect(
        species,
        fixed("Anagotus indet. 'lewisi (Broun)' group", ignore_case = TRUE)
      ) ~
        "Anagotus sp.",
      # Everything else passes through (canonical names included)
      TRUE ~ species
    )
  ) |>
  select(-generic_name, -specific_name) |>
  mutate(species = unname(species)) |> # drop the names attribute
  separate_wider_delim(
    cols = species,
    delim = " ",
    names = c("generic_name", "specific_name"),
    too_many = "merge", # handles "Anagotus indet." staying intact in specific_name
    cols_remove = FALSE # keep the original species column
  ) |>
  mutate(year = lubridate::ymd(year, truncated = 2L)) |>
  filter(
    data_origin %in%
      c(
        "nwb_nzac_digitised",
        "nwb_nhm_digitised",
        "nwb_anic_digitised",
        "nwb_monz_digitised",
        "tfbis_lunz_digitised",
        "tfbis_cmnz_digitised",
        "tfbis_frnz_digitised",
        "tfbis_omnz_digitised",
        "gbif_ak_digitised",
        "gbif_nzac_digitised"
      )
  ) |>
  # assign UNK if both institution_code and catalog_number are NA
  mutate(
    institution_code = if_else(
      is.na(catalog_number) & is.na(institution_code),
      "UNK",
      institution_code
    )
  ) |>
  # impute temporary catalog_number for NA records
  group_by(institution_code) |>
  mutate(
    .seq_tmp = if_else(is.na(catalog_number), row_number(), NA_integer_)
  ) |>
  ungroup() |>
  mutate(
    catalog_number = if_else(
      is.na(catalog_number),
      str_c(
        institution_code,
        "_",
        str_pad(.seq_tmp, width = 4, pad = "0"),
        "_temp"
      ),
      catalog_number
    )
  ) |>
  select(-.seq_tmp) |>
  distinct(catalog_number, .keep_all = TRUE) |>
  select(-id) |>
  filter(!institution_code %in% c("PBDB", "brow")) |> # remove institutions with <3 records
  mutate(
    availability = case_when(
      source == "gbif" ~ "GBIF",
      source == "anagotus.data" &
        institution_code %in%
          c(
            "LUNZ",
            "NZAC",
            "NHMUK",
            "CMNZ",
            "MONZ",
            "ANIC",
            "OMNZ",
            "FRNZ"
          ) ~ "non-GBIF",
      .default = NA_character_
    ),
    data_type = "occurrence_record"
  )


# CoordinateCleaner check ------------------------------------------------

# flags <- CoordinateCleaner::clean_coordinates(
#   df_anagotus_combined,
#   lon = "decimal_longitude",
#   lat = "decimal_latitude",
#   species = "species"
# ) |>
#   filter(.summary == FALSE)

# Geospatial data --------------------------------------------------------

# NZ Data Stack ----------------------------------------------------------

tiff_files <- list.files(
  path = here::here("data", "nzenvds_v1.1", "final_layers_nztm"),
  pattern = "\\.tif$",
  full.names = TRUE
)

stack_nz_env_data <- terra::rast(tiff_files)

# Bring into sf object with WGS84 CRS (lat-long coords), then transform to NZTM2000 (EPSG:2193)
sf_anagotus_combined <- df_anagotus_combined |>
  sf::st_as_sf(
    coords = c("decimal_longitude", "decimal_latitude"),
    crs = 4326
  ) |>
  sf::st_transform(crs = 2193)


# extract environmental data ---------------------------------------------

df_enviro_extract <- terra::extract(stack_nz_env_data, sf_anagotus_combined)
df_anagotus_environment <- cbind(sf_anagotus_combined, df_enviro_extract)

# spatial sample of random NZ points -------------------------------------

## Code used below - commented out to reduce computation time:
# set.seed(123)
# stack_nz_env_data |>
#   terra::spatSample(size = 10000, method = "random", as.df = TRUE, na.rm = TRUE, xy = TRUE, exhaustive = TRUE) |>
#   write_rds(file = here::here("data", "nz_spat_random_sample.rds"))

sf_nz_random_sample_10k <- read_rds(here::here(
  "data",
  "nz_spat_random_sample.rds"
)) |>
  label_bg_sample("sf_nz_random_sample_10k") |>
  sf::st_as_sf(coords = c("x", "y"), crs = 2193)

df_anagotus_environment_10k <- bind_rows(
  df_anagotus_environment,
  sf_nz_random_sample_10k
)

# DoC 10K DF prep --------------------------------------------------------

sf_doc_random_sample_10k <- get_doc_env_sample(
  n = 10000,
  seed = 124,
  path = here::here("data", "sf_doc_random_sample_10k")
) |>
  label_bg_sample("sf_doc_random_sample_10k") |>
  sf::st_as_sf(coords = c("x", "y"), crs = 2193)

# map specimens relative to DoC polygons ---------------------------------

gpkg_file <- here::here(
  "data",
  "DOC_Public_Conservation_Land_4011045003548110480.gpkg"
)

doc_polys <- sf::st_read(gpkg_file, quiet = TRUE) |>
  sf::st_transform(crs = 2193)

# bind points, then attach doc_polys$Name by spatial join (NA if outside polygon)
sf_transformed <- bind_rows(
  df_anagotus_environment_10k,
  sf_doc_random_sample_10k
) |>
  sf::st_join(
    doc_polys |>
      dplyr::select(Name) |>
      sf::st_transform(sf::st_crs(bind_rows(
        df_anagotus_environment_10k,
        sf_doc_random_sample_10k
      ))),
    left = TRUE
  )

# save the sf points as a matrix of coordinates
coords <- sf::st_coordinates(sf_transformed)

df_pca_for_all <- sf_transformed |>
  dplyr::mutate(
    decimal_longitude = coords[, 1],
    decimal_latitude = coords[, 2]
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(
    institution_code,
    catalog_number,
    location,
    source,
    specific_name,
    availability,
    data_type,
    decimal_latitude,
    decimal_longitude,
    temp_meanAnn,
    temp_minJuly,
    vpd_oct,
    waterBalance_annDeficit,
    waterBalance_R2PET
  ) |>
  tidyr::drop_na(
    temp_meanAnn,
    temp_minJuly,
    vpd_oct,
    waterBalance_annDeficit,
    waterBalance_R2PET
  )


# species for maps -------------------------------------------------------

species_keep <- df_pca_for_all |>
  filter(!str_detect(specific_name, "species_|sp\\.")) |>
  count(specific_name) |>
  filter(n >= 10) |>
  pull(specific_name)

# RColorBrewer palette ---------------------------------------------------

set1_custom <- brewer.pal(9, "Set1")
set1_custom[9] <- "#009E73"


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


# 3. Nest counts by species ----------------------------------------------

counts_nested <- counts |>
  nest(data = -specific_name)

# Genus Counts for ecological regions ------------------------------------

# 1. Occurrence counts per region x source (genus level) -----------------

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
