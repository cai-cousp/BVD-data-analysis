# Script 3: Spatial Mapping against Capacity

suppressPackageStartupMessages({
  library(sf)
  library(readxl)
})

source(here::here("R/alert_helpers.R"))

# 1. Paths
data_folder <- file.path(evd17_root, "DataCleaning", "data", "Output")
output_dir <- create_output_dir(here::here("output"))
adequacy_path <- file.path(output_dir, "02_recent_adequacy.xlsx")
trends_smooth_path <- file.path(output_dir, "02_trends_smooth.rds")
access_path <- file.path(evd17_root, "DataAnalysis", "data", "20250701 RDC_ZS_Accessibilité.xlsx")

# 2. Load Data
message("Loading mapping data...")
if (!file.exists(adequacy_path) || !file.exists(trends_smooth_path)) {
  rlang::abort("Adequacy or trends data missing. Run 02_alert_trends.R first.")
}

adequacy <- readxl::read_excel(adequacy_path)
trends <- readRDS(trends_smooth_path)
access <- readxl::read_excel(access_path)

# Spatial data (load from provincial GPKG files)
map_base <- file.path(evd17_root, "Maps", "health_zones")
hz_sf <- tryCatch({
  ituri <- sf::read_sf(file.path(map_base, "GRID3_COD_Ituri_health_zones_v8_0.gpkg"))
  nk    <- sf::read_sf(file.path(map_base, "GRID3_COD_Nord-Kivu_health_zones_v8_0.gpkg"))
  sk    <- sf::read_sf(file.path(map_base, "GRID3_COD_Sud-Kivu_health_zones_v8_0.gpkg"))
  dplyr::bind_rows(ituri, nk, sk)
}, error = function(e) {
  message("Could not load shapefiles directly. Make sure they are available in Maps/health_zones.")
  NULL
})

if (is.null(hz_sf)) {
  message("Warning: Failed to load spatial data. Maps will not be generated, but dashboard will be created.")
}

# 3. Phase 1 & 2: Capacity Overlay and Mapping
message("Merging spatial and capacity data...")
access_clean <- access |>
  dplyr::select(zone_sante_notification = Zone_de_sante, Acces) |>
  dplyr::mutate(
    zone_sante_notification = stringr::str_to_title(tolower(zone_sante_notification))
  )

# 4. Phase 3: Bivariate Map / Plots
if (!is.null(hz_sf)) {
  message("Creating maps...")
  
  # Determine the HZ name column in shapefile
  hz_col <- names(hz_sf)[grep("name|zone", tolower(names(hz_sf)))][1]
  
  map_data <- hz_sf |>
    dplyr::mutate(
      zone_sante_notification = stringr::str_to_title(tolower(.data[[hz_col]]))
    ) |>
    dplyr::left_join(adequacy, by = "zone_sante_notification") |>
    dplyr::left_join(access_clean, by = "zone_sante_notification")

  pdf(file.path(output_dir, "03_alert_maps.pdf"), width = 10, height = 8)

  p1 <- ggplot(map_data) +
    geom_sf(aes(fill = adequacy_category), color = "white", linewidth = 0.2) +
    scale_fill_manual(
      values = c("Under-alerting" = "firebrick", "Adequate" = "palegreen4", "Over-alerting" = "goldenrod"),
      na.value = "grey90"
    ) +
    theme_void() +
    labs(title = "Alert Adequacy by Health Zone", fill = "Adequacy")
  print(p1)

  p2 <- ggplot(map_data) +
    geom_sf(aes(fill = Acces), color = "white", linewidth = 0.2) +
    theme_void() +
    labs(title = "Accessibility Status by Health Zone", fill = "Accessibility")
  print(p2)

  dev.off()
}

# 5. Phase 4: Performance Dashboard Table
message("Generating performance dashboard...")
dashboard <- adequacy |>
  dplyr::left_join(access_clean, by = "zone_sante_notification") |>
  dplyr::select(
    `Health Zone` = zone_sante_notification,
    `Trend Direction` = trend_direction,
    `Adequacy Category` = adequacy_category,
    `Mean AAI` = mean_aai,
    Accessibility = Acces
  )

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(dashboard, file.path(output_dir, "03_performance_dashboard.xlsx"))
}

message("Mapping and dashboard complete. Output saved to ", output_dir)
