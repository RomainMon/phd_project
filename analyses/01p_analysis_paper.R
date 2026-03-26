#------------------------------------------------#
# Authors: Romain Monassier, Valéria Romano, Anne-Marie Farnet Da Silva
# Script accompanying the paper entitled: "..."
#------------------------------------------------#

library(here)

# 0. Data -------
## Landscape metrics
base_path = here("outputs", "data", "landscapemetrics")
# All land uses
all_lulc_metrics = readr::read_csv(
  file.path(base_path, "all_lulc_classes_bbox_1989_2100.csv"),
  show_col_types = FALSE)
# Metrics for forest class only
forest_class_metrics = readr::read_csv(
  file.path(base_path, "forest_class_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE)
# Forest age
forest_age_metrics = readr::read_csv(
  file.path(base_path, "forest_age_metrics_bbox_1989_2024.csv"),
  show_col_types = FALSE)

## Modeling datasets
base_path = here("outputs", "data", "MapBiomas", "LULCC_datasets")
# Property-scale deforestation dataset

# 1. Landscape changes ----------

# 2. Drivers of forest change ----------

# 3. Hotspots of reforestation and deforestation ----------

# 4. Simulations ----------