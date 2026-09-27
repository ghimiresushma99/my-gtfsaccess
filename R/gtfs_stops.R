#' Read GTFS Stops
#'
#' Reads a GTFS feed and returns the stop locations as a
#' `gtfs_stops` object.
#'
#' @param gtfs_zip Path to a GTFS ZIP file.
#'
#' @return A `gtfs_stops` object.
#' @export
#' 
#' @examples
#' \dontrun{
#' stops <- read_gtfs_stops("muenster_gtfs.zip")
#'
#' print(stops)
#' summary(stops)
#' plot(stops)
#' }
read_gtfs_stops <- function(gtfs_zip) {

  if (!file.exists(gtfs_zip)) {
    stop("GTFS ZIP file does not exist.")
  }

  gtfs <- tidytransit::read_gtfs(gtfs_zip)

  if (is.null(gtfs$stops)) {
    stop("GTFS feed does not contain a stops table.")
  }

  stops <- gtfs$stops

  required_cols <- c(
    "stop_id",
    "stop_lon",
    "stop_lat"
  )

  if (!all(required_cols %in% names(stops))) {
    stop(
      "Stops table must contain stop_id, stop_lon, and stop_lat columns."
    )
  }

  stops_sf <- sf::st_as_sf(
    stops,
    coords = c("stop_lon", "stop_lat"),
    crs = 4326,
    remove = FALSE # Only the geometry is transformed to EPSG:3035. Keep the original WGS84 longitude/latitude columns.
  )

  # Remove duplicate stop locations
  coords <- sf::st_coordinates(stops_sf)

  stops_sf <- stops_sf[
    !duplicated(coords),
  ]

  # Project Stop for analysis
  stops_sf <- sf::st_transform(
    stops_sf,
    3035 
  )
  
  new_gtfs_stops(
    stops = stops_sf
  )
}

#' Stop Statistics
#'
#' Calculates descriptive statistics for GTFS stops within a
#' study area.
#'
#' @param x A `gtfs_stops` object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list containing stop statistics.
#' @export
#' 
#' @examples
#' \dontrun{
#' stops <- read_gtfs_stops("muenster_gtfs.zip")
#'
#' stats <- stop_statistics(
#'   stops,
#'   geojson_file = "study_area.geojson"
#' )
#'
#' stats
#' }
stop_statistics <- function(x, ...) {
  UseMethod("stop_statistics")
}

#' @rdname stop_statistics
#' @param geojson_file Path to the study area boundary.
#' @export
stop_statistics.gtfs_stops <- function(x, geojson_file, ...) {

  number_of_stops <- count_gtfs_stops(
    x$stops
  )

  area_km2 <- calculate_geojson_area(
    geojson_file
  )

  density <- calculate_stop_density(
    number_of_stops,
    area_km2
  )

  study_area <- tools::file_path_sans_ext(
    basename(geojson_file)
  )

  stats <- list(
    study_area = study_area,
    number_of_stops = density$number_of_stops,
    area_km2 = density$area_km2,
    stops_per_km2 = density$stops_per_km2,
    km2_per_stop = density$km2_per_stop
  )

  cat("GTFS Stop Statistics\n")
  cat("---------------------\n")
  cat("Study area:", stats$study_area, "\n")
  cat("Number of stops:", stats$number_of_stops, "\n")
  cat("Area:", round(stats$area_km2, 2), "sq.km\n")
  cat(
    "Stop density:",
    round(stats$stops_per_km2, 2),
    "stops per sq.km\n"
  )
  cat(
    "Area per stop:",
    round(stats$km2_per_stop, 2),
    "per sq.km\n"
  )

  invisible(stats)
}


#' Create Service Areas
#'
#' Creates Voronoi service areas from GTFS stop locations.
#'
#' @param x A `gtfs_stops` object. Stops with identical coordinates are
#' represented once to support Voronoi service area generation.
#' @param ... Additional arguments passed to methods.
#'
#' @return An sf object containing service area polygons.
#' @export
create_service_areas <- function(x, ...) {
  UseMethod("create_service_areas")
}

#' @rdname create_service_areas
#' @param geojson_file Path to the study area boundary.
#' @param plot Logical. If TRUE, plots the service areas and stops.
#' @export
#' 
#' @examples
#' \dontrun{
#' stops <- read_gtfs_stops("muenster_gtfs.zip")
#'
#' service_areas <- create_service_areas(
#'   stops,
#'   geojson_file = "study_area.geojson"
#' )
#'
#' plot(service_areas["stop_id"])
#' }
create_service_areas.gtfs_stops <- function(
  x,
  geojson_file,
  plot = FALSE,
  ...
) {

  if (!file.exists(geojson_file)) {
    stop("GeoJSON file does not exist.")
  }

  boundary <- sf::st_read(
    geojson_file,
    quiet = TRUE
  )

  stops_sf <- x$stops

  boundary <- sf::st_transform(
    boundary,
    sf::st_crs(stops_sf)
  )

  voronoi <- sf::st_voronoi(
    sf::st_union(
      sf::st_geometry(stops_sf)
    )
  )

  voronoi <- sf::st_collection_extract(
    voronoi,
    "POLYGON"
  )

  voronoi <- sf::st_sf(
    geometry = voronoi,
    crs = sf::st_crs(stops_sf)
  )

  # Match each Voronoi cell to its generating stop by containment
  voronoi_idx <- sf::st_nearest_feature(stops_sf, voronoi)
  voronoi <- voronoi[voronoi_idx, ]

  voronoi$stop_id <- stops_sf$stop_id

  if ("stop_name" %in% names(stops_sf)) {
    voronoi$stop_name <- stops_sf$stop_name
  }

  service_areas <- sf::st_intersection(
    voronoi,
    boundary
  )

  if (plot) {

    plot(
      sf::st_geometry(service_areas),
      main = "GTFS Service Areas"
    )

    plot(
      sf::st_geometry(x$stops),
      add = TRUE,
      pch = 16,
      col = "red"
    )

  }

  service_areas
}

# -------------------------------------------
# HELPER FUNCTIONS
# -------------------------------------------

#' Count GTFS Stops
#'
#' Counts the number of unique GTFS stops.
#'
#' @param stops A GTFS stops data frame.
#'
#' @return Number of unique stops.
#' @export
#' 
#' @examples
#' \dontrun{
#' stops <- read_gtfs_stops("muenster_gtfs.zip")
#'
#' count_gtfs_stops(stops$stops)
#' }
count_gtfs_stops <- function(stops) {

  if (!is.data.frame(stops)) {
    stop("stops must be a data frame.")
  }

  if (!"stop_id" %in% names(stops)) {
    stop("stops must contain a stop_id column.")
  }

  length(unique(stops$stop_id))
}


#' Calculate GeoJSON Area
#'
#' Calculates the study area in square kilometres.
#'
#' @param geojson_file Path to a GeoJSON file.
#'
#' @return Area in square kilometres.
#' @export
#' 
#' @examples
#' \dontrun{
#' calculate_geojson_area("study_area.geojson")
#' }
calculate_geojson_area <- function(geojson_file) {

  if (!file.exists(geojson_file)) {
    stop("GeoJSON file does not exist.")
  }

  boundary <- sf::st_read(
    geojson_file,
    quiet = TRUE
  )

  boundary <- sf::st_transform(
    boundary,
    3035
  )

  as.numeric(
    sum(sf::st_area(boundary))
  ) / 1e6
}


#' Calculate Stop Density
#'
#' Calculates stop density statistics.
#'
#' @param number_of_stops Number of stops.
#' @param area_km2 Study area in square kilometres.
#'
#' @return A list of stop density statistics.
#' @export
#' 
#' @examples
#' \dontrun{
#' density <- calculate_stop_density(
#'   number_of_stops = 500,
#'   area_km2 = 100
#' )
#'
#' density
#' }
calculate_stop_density <- function(
  number_of_stops,
  area_km2
) {

  if (!is.numeric(number_of_stops) ||
      !is.numeric(area_km2) ||
      length(number_of_stops) != 1 ||
      length(area_km2) != 1) {
    stop(
      "number_of_stops and area_km2 must be single numeric values."
    )
  }

  if (number_of_stops <= 0 || area_km2 <= 0) {
    stop(
      "number_of_stops and area_km2 must be greater than zero."
    )
  }

  list(
    number_of_stops = number_of_stops,
    area_km2 = area_km2,
    stops_per_km2 = number_of_stops / area_km2,
    km2_per_stop = area_km2 / number_of_stops
  )
}

