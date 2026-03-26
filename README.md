# nzjz_dark_data

Code archive for the analysis of "dark data" in New Zealand entomological collections, using the weevil genus *Anagotus* as a case study. The paper compares occurrence records available on GBIF (prior to this study) with those held in institutional collections but not yet digitised, not shared online, or online but not easily accessible, quantifying the impact of these "dark" specimen data on spatial and environmental coverage.

> **Citation**
> *[Author(s)]. (Year). [Paper title]. New Zealand Journal of Zoology, [volume(issue)], [pages]. https://doi.org/[DOI]*
>


## Software requirements

| Requirement | Version |
|---|---|
| R | ≥ 4.5.2 |
| renv | 1.2.0 (managed automatically) |

The project uses [renv](https://rstudio.github.io/renv/) to manage package dependencies. All required packages and their exact versions are recorded in `renv.lock`.

### Setup

```r
# 1. Open the project in your IDE of choice (e.g. RStudio, VS Code)
# 2. renv will bootstrap itself on first load. Then restore all packages:
renv::restore()
```

Key packages include `tidyverse`, `sf`, `terra`, `rgbif`, `cowplot`, `patchwork`, `flextable`, and the project-specific `anagotus.data` (installed from a Git repository).

## Running the analysis

Scripts are in `code/` and should be run in order. Each script sources `00_setup.R`, which loads libraries and helper functions.

| Order | Script | Description |
|---|---|---|
| 0 | `00_setup.R` | Loads all libraries and sources `01_functions.R`. Sourced automatically by every downstream script. |
| 1 | `01_functions.R` | Defines helper functions (species-name parsing, custom PCA plotting via `nnpca()`, map themes, etc.). Not run directly. |
| 2 | `02_download_data.R` | Downloads/checks the four external datasets (see [Data provenance](#data-provenance) below). **Run once** before any analysis. |
| 3 | `03_df_preparation.R` | Imports GBIF and `anagotus.data` records, cleans taxonomy, merges data sources, extracts environmental variables from rasters, generates background random samples, and prepares objects used by later scripts. |
| 4 | `04_summary_stats.R` | Produces summary statistics and the observations-by-decade figure. |
| 5 | `05_ecological_districts_genus.R` | Maps genus-level occurrence counts across NZ ecological regions (GBIF vs. non-GBIF). |
| 6 | `06_ecological_districts_species.R` | Maps species-level occurrence counts across NZ ecological regions for species with ≥ 10 records. |
| 7 | `07_niche_space_anagotus.R` | PCA of environmental niche space for all *Anagotus* occurrences with biplot loadings. |
| 8 | `08_niche_space_source.R` | PCA of environmental niche space coloured by data availability (GBIF vs. non-GBIF). |
| 9 | `09_niche_space_institutions.R` | PCA of environmental niche space coloured by holding institution. |

```bash
# Quickstart (from the R console, with the project open):
source("code/02_download_data.R")   # once, to fetch/check data
source("code/03_df_preparation.R")  # prepare analysis objects
source("code/04_summary_stats.R")
source("code/05_ecological_districts_genus.R")
source("code/06_ecological_districts_species.R")
source("code/07_niche_space_anagotus.R")
source("code/08_niche_space_source.R")
source("code/09_niche_space_institutions.R")
```

## Data provenance

### 1. GBIF occurrence data

- **What:** Occurrence records for *Anagotus* (Coleoptera: Curculionidae) from the Global Biodiversity Information Facility.
- **Download key:** `0025977-251009101135966`
- **Citation:** GBIF.org (15 October 2025) GBIF Occurrence Download <https://doi.org/10.15468/dl.bd6k4h>
- **How to obtain:** Downloaded automatically by `02_download_data.R` using `rgbif::occ_download_get()`. Requires GBIF credentials (`GBIF_USER`, `GBIF_PWD`, `GBIF_EMAIL` environment variables). See the [rgbif credentials guide](https://docs.ropensci.org/rgbif/articles/gbif_credentials.html).
- **Licence:** Subject to GBIF data-use terms — <https://www.gbif.org/terms>.
- **Location:** `data/0025977-251009101135966.zip`

### 2. anagotus.data (non-GBIF specimen records)

- **What:** Curated dataset of *Anagotus* specimen records held in NZ and international collections some of which is not available on GBIF.
- **How to obtain:** Installed as an R package from a private Git repository: `remotes::install_git("git@github.com:nbir012/anagotus.data.git")`.
- **Licence:** tbc.

### 3. NZ Environmental Data Stack (nzenvds v1.1)

- **What:** A stack of 72 bioclimatic raster layers for New Zealand at 25 m resolution. This project uses 5 layers: `temp_meanAnn`, `temp_minJuly`, `vpd_oct`, `waterBalance_annDeficit`, `waterBalance_R2PET`.
- **Source:** BSI (formerly Manaaki Whenua – Landcare Research.)
- **DOI:** <https://dx.doi.org/10.20417/nzjecol.45.31>
- **How to obtain:** Manual download from [New Zealand Environmental Data Stack](https://datastore.landcareresearch.co.nz/dataset/486076fc-878a-42af-b245-889e4dc3ad1e/resource/13360a13-c478-4d7d-980f-3db44a749fa7/download/nzenvds_v1.1_finalnztm.zip). Download as GeoTIFF (NZTM projection) and extract so that `.tif` files are in `data/nzenvds_v1.1/final_layers_nztm/`.
- **Licence:** [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
- **Location:** `data/nzenvds_v1.1/final_layers_nztm/`

### 4. Ecological Districts

- **What:** Spatial polygons of NZ ecological districts/regions.
- **Source:** Department of Conservation
- **How to obtain:** Download the GeoPackage from <https://doc-deptconservation.opendata.arcgis.com/datasets/4f40397b253646f0a2ac6898ff4012c5_0/about> and place it at `data/Ecological_Districts_-2969373808597425294.gpkg`.
- **Licence:** [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/).
- **Location:** `data/Ecological_Districts_-2969373808597425294.gpkg`

### Pre-computed data files (included in the repository)

| File | Description |
|---|---|
| `data/df_added_coords` | Manually georeferenced coordinates for GBIF records that lacked latitude/longitude. |
| `data/nz_spat_random_sample.rds` | 10 000 spatially random points across NZ with extracted environmental values (seed = 123). |

## Expected outputs

All outputs are written to `output/`.

### Figures (`output/figures/`)

| File | Script | Description |
|---|---|---|
| `observations_by_year.png` | `04_summary_stats.R` | Stacked bar chart of specimen observations by decade and institution. |
| `genus_figure.png` | `05_ecological_districts_genus.R` | Heatmaps of genus-level GBIF, combined, and dark-data counts by ecological region. |
| `combined_heatmaps_p1–p4.png` | `06_ecological_districts_species.R` | Per-species heatmaps of occurrence counts by ecological region. |
| `fig_anagotus_pca.png` | `07_niche_space_anagotus.R` | PCA biplot of environmental niche space for all *Anagotus*. |
| `fig_availability_pca.png` | `08_niche_space_source.R` | PCA coloured by data source (GBIF vs. non-GBIF). |
| `fig_institution_pca.png` | `09_niche_space_institutions.R` | PCA coloured by holding institution. |

### Tables (`output/tables/`)

| File | Description |
|---|---|
| `online_offline_specimens.docx` | Counts of digitised vs. non-digitised specimens by institution. |
| `region_coverage_genus.docx` | Ecological-region coverage summary at the genus level. |
| `region_coverage_species.docx` | Ecological-region coverage summary at the species level. |

## Licence

*[tbc]*
