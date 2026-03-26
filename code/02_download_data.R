# 0_download_data.R
# Download and prepare external datasets required by the analysis.
#
# Some datasets must be downloaded manually from their respective portals
# (see instructions below). This script handles:
#
#   1. GBIF occurrence data (automated via rgbif)
#   2. NZ Environmental Data Stack — nzenvds v1.1 (manual download, subset)
#   3. DOC Public Conservation Land (manual download)
#   4. Ecological Districts (manual download)
#
# Run this script ONCE before running 1_df_preparation.R.

source(here::here("code", "00_setup.R"))

# ── 1. GBIF occurrence download ──────────────────────────────────────────────
#
# Requires GBIF credentials. Set these environment variables before running:
#   GBIF_USER, GBIF_PWD, GBIF_EMAIL
# See: https://docs.ropensci.org/rgbif/articles/gbif_credentials.html

gbif_zip <- here("data", "0025977-251009101135966.zip")

if (!file.exists(gbif_zip)) {
  message("Downloading GBIF occurrence data...")
  occ_download_get(
    key = "0025977-251009101135966",
    path = here("data")
  )
  message("GBIF download complete.")
} else {
  message("GBIF data already present: ", gbif_zip)
}


# ── 2. NZ Environmental Data Stack (nzenvds v1.1) ───────────────────────────
#
# Source: Manaaki Whenua – Landcare Research
#   https://lris.scinfo.org.nz/layer/105090-nz-environmental-data-stack-nzenvds-v11/
#   DOI: https://doi.org/10.7931/nt8p-ck76
#
# MANUAL STEP: Download the dataset from the LRIS portal above.
#   - You will need to create a free account.
#   - Download as GeoTIFF (NZTM projection).
#   - Extract the archive so that .tif files are in:
#       data/nzenvds_v1.1/final_layers_nztm/
#
# This project only uses 5 of the 72 layers. After downloading, you can
# optionally delete the unused layers to save ~4.7 GB of disk space.

nzenvds_dir <- here("data", "nzenvds_v1.1", "final_layers_nztm")

# Layers required by this analysis
required_layers <- c(
  "temp_meanAnn",
  "temp_minJuly",
  "vpd_oct",
  "waterBalance_annDeficit",
  "waterBalance_R2PET"
)

if (dir.exists(nzenvds_dir)) {
  present <- tools::file_path_sans_ext(
    list.files(nzenvds_dir, pattern = "\\.tif$")
  )
  missing <- setdiff(required_layers, present)
  if (length(missing) > 0) {
    warning(
      "Missing nzenvds layers: ",
      paste(missing, collapse = ", "),
      "\nDownload the full dataset from LRIS and extract to: ",
      nzenvds_dir
    )
  } else {
    message("All required nzenvds layers present.")
  }
} else {
  warning(
    "nzenvds directory not found: ",
    nzenvds_dir,
    "\nDownload from: https://lris.scinfo.org.nz/layer/105090-nz-environmental-data-stack-nzenvds-v11/"
  )
}


# ── 3. DOC Public Conservation Land ──────────────────────────────────────────
#
# Source: Department of Conservation / data.govt.nz
#   https://data.govt.nz/dataset/doc-public-conservation-land/
#
# MANUAL STEP: Download the GeoPackage (.gpkg) and place it in:
#   data/DOC_Public_Conservation_Land_4011045003548110480.gpkg

gpkg_doc <- here(
  "data",
  "DOC_Public_Conservation_Land_4011045003548110480.gpkg"
)

if (file.exists(gpkg_doc)) {
  message("DOC conservation land data present.")
} else {
  warning(
    "DOC conservation land GPKG not found: ",
    gpkg_doc,
    "\nDownload from: https://data.govt.nz/dataset/doc-public-conservation-land/"
  )
}


# ── 4. Ecological Districts ─────────────────────────────────────────────────
#
# Source: Department of Conservation / data.govt.nz
#   https://data.govt.nz/dataset/nz-ecological-districts/
#
# MANUAL STEP: Download the GeoPackage (.gpkg) and place it in:
#   data/Ecological_Districts_-2969373808597425294.gpkg

gpkg_eco <- here("data", "Ecological_Districts_-2969373808597425294.gpkg")

if (file.exists(gpkg_eco)) {
  message("Ecological districts data present.")
} else {
  warning(
    "Ecological districts GPKG not found: ",
    gpkg_eco,
    "\nDownload from: https://data.govt.nz/dataset/nz-ecological-districts/"
  )
}


# ── Summary ──────────────────────────────────────────────────────────────────

message("\n=== Data download check complete ===")
message(
  "If any warnings appeared above, follow the manual download instructions."
)
message(
  "Once all data are in place, proceed with: source('code/1_df_preparation.R')"
)
