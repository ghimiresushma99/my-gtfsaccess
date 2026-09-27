#' Create a GTFS Stops Object
#'
#' Creates an S3 object representing GTFS stop locations.
#'
#' @param stops An sf object containing GTFS stop locations.
#'
#' @return An object of class `gtfs_stops`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' stops <- data.frame(
#'   stop_id = c("S1", "S2"),
#'   stop_name = c("Central Station", "University"),
#'   stop_lon = c(7.6261, 7.5952),
#'   stop_lat = c(51.9607, 51.9694)
#' )
#'
#' stops_sf <- st_as_sf(
#'   stops,
#'   coords = c("stop_lon", "stop_lat"),
#'   crs = 4326
#' )
#'
#' obj <- new_gtfs_stops(stops_sf)
#'
#' print(obj)
#' summary(obj)
#' }

new_gtfs_stops <- function(stops) {

  if (!inherits(stops, "sf")) {
    stop("stops must be an sf object.")
  }

  required_cols <- c("stop_id","stop_lon","stop_lat")

  if (!all(required_cols %in% names(stops))) {
    stop("stops must contain stop_id and geometry columns.")
  }

  obj <- list(
    stops = stops
  )

  class(obj) <- c("gtfs_stops", "list")

  obj
}

#' Create a GTFS Accessibility Object
#'
#' Creates an S3 object representing accessibility results
#' derived from GTFS service areas and population data.
#'
#' @param accessibility An sf object containing accessibility results.
#'
#' @return An object of class `gtfs_accessibility`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' accessibility <- st_sf(
#'   stop_id = c("S1", "S2"),
#'   population_in_catchment = c(1250, 890),
#'   departures = c(320, 180),
#'   accessibility_index = c(0.256, 0.202),
#'   geometry = st_sfc(
#'     st_polygon(list(rbind(
#'       c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
#'     ))),
#'     st_polygon(list(rbind(
#'       c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)
#'     )))
#'   ),
#'   crs = 4326
#' )
#'
#'
#' obj <- new_gtfs_accessibility(
#'   accessibility,
#' )
#'
#' print(obj)
#' summary(obj)
#' }
new_gtfs_accessibility <- function(
  accessibility
) {

  if (!inherits(accessibility, "sf")) {
    stop("accessibility must be an sf object.")
  }

  required_cols <- c(
    "stop_id",
    "population_in_catchment",
    "departures",
    "accessibility_index"
  )

  if (!all(required_cols %in% names(accessibility))) {
    stop(
      "accessibility must contain stop_id, ",
      "population_in_catchment, departures, ",
      "and accessibility_index columns."
    )
  }

  obj <- list(
    accessibility = accessibility
  )

  class(obj) <- c("gtfs_accessibility", "list")

  obj
}

#' Create a GTFS District Accessibility Object
#'
#' Creates an S3 object representing accessibility results aggregated
#' to district level. This is the object returned by
#' \code{\link{aggregate_accessibility}}; you generally won't need to
#' call this constructor directly.
#'
#' @param accessibility An sf object containing district-level
#'   aggregated accessibility results.
#'
#' @return An object of class `gtfs_district_accessibility`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' districts <- st_sf(
#'   district_name = c("North", "South"),
#'   mean_accessibility = c(0.21, 0.14),
#'   total_population = c(4200, 3100),
#'   total_departures = c(950, 480),
#'   geometry = st_sfc(
#'     st_polygon(list(rbind(
#'       c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
#'     ))),
#'     st_polygon(list(rbind(
#'       c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)
#'     )))
#'   ),
#'   crs = 4326
#' )
#'
#' obj <- new_gtfs_district_accessibility(districts)
#'
#' print(obj)
#' plot(obj)
#' }
new_gtfs_district_accessibility <- function(accessibility) {

  if (!inherits(accessibility, "sf")) {
    stop("accessibility must be an sf object.")
  }

  required_cols <- c(
    "district_name",
    "mean_accessibility",
    "total_population",
    "total_departures"
  )

  if (!all(required_cols %in% names(accessibility))) {
    stop(
      "accessibility must contain district_name, mean_accessibility, ",
      "total_population, and total_departures columns."
    )
  }

  obj <- list(
    accessibility = accessibility
  )

  class(obj) <- c("gtfs_district_accessibility", "list")

  obj
}