#' Calculate Accessibility
#'
#' Calculates an accessibility index for each GTFS service area
#' based on the population living within the catchment and the
#' number of departures serving each stop.
#'
#' @param service_areas An sf object returned by
#'   create_service_areas().
#' @param population A SpatRaster containing population counts.
#' @param gtfs_zip Path to a GTFS ZIP file.
#'
#' @return A gtfs_accessibility object.
#' @export
#' 
#' @examples
#' \dontrun{
#' gtfs_zip <- "muenster_gtfs.zip"
#' 
#' stops <- read_gtfs_stops(gtfs_zip)
#'
#' service_areas <- create_service_areas(
#'   stops,
#'   "boundary.geojson"
#' )
#'
#' population <- terra::rast("population.tif")
#'
#' accessibility <- calculate_accessibility(
#'   service_areas,
#'   population,
#'   gtfs_zip
#' )
#'
#' summary(accessibility)
#' plot(accessibility)
#' }
calculate_accessibility <- function(
  service_areas,
  population,
  gtfs_zip
) {

  if (!inherits(service_areas, "sf")) {
    stop("service_areas must be an sf object.")
  }

  if (!file.exists(gtfs_zip)) {
    stop("GTFS ZIP file does not exist.")
  }

  # Population within each catchment
  service_areas <- extract_population(
    service_areas,
    population
  )

  # Count departures
  departures <- count_gtfs_departures(gtfs_zip)

  # Join departures
  service_areas <- join_departures(
    service_areas,
    departures
  )

  # Accessibility index: departures per resident within the catchment.
  # Catchments with zero population are left as NA rather than Inf/0,
  # since "accessibility per person" is undefined when there is no one
  # there to be served.
  service_areas$accessibility_index <- ifelse(
    service_areas$population_in_catchment > 0,
    (service_areas$departures /
      service_areas$population_in_catchment) * 100,
    NA_real_
  )

  new_gtfs_accessibility(
    accessibility = service_areas
  )
}


#' @importFrom rlang .data
NULL

#' Accessibility Statistics
#'
#' Returns summary statistics for a GTFS accessibility object.
#'
#' @param x A gtfs_accessibility object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A list of accessibility statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- accessibility_statistics(accessibility)
#'
#' print(stats)
#' }
accessibility_statistics <- function(x, ...) {
  UseMethod("accessibility_statistics")
}

#' @rdname accessibility_statistics
#' @export
accessibility_statistics.gtfs_accessibility <- function(x, ...) {

  data <- x$accessibility

  list(
    mean_accessibility =
      mean(data$accessibility_index, na.rm = TRUE),
    median_accessibility =
      stats::median(data$accessibility_index, na.rm = TRUE),
    min_accessibility =
      min(data$accessibility_index, na.rm = TRUE),
    max_accessibility =
      max(data$accessibility_index, na.rm = TRUE)
  )
}


#' Aggregate Accessibility
#'
#' Aggregates accessibility to larger districts polygons.
#'
#' @param x A gtfs_accessibility object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A gtfs_district_accessibility object containing aggregated
#'   accessibility statistics for each district.
#' @export
#'
#' @examples
#' \dontrun{
#' districts <- system.file(
#'   "extdata",
#'   "districts.geojson",
#'   package = "gtfsaccess"
#' )
#'
#' district_accessibility <-
#'   aggregate_accessibility(
#'     accessibility,
#'     districts
#'   )
#' }
aggregate_accessibility <- function(x, ...) {
  UseMethod("aggregate_accessibility")
}

#' @rdname aggregate_accessibility
#' @param districts An sf polygon layer for districts or a path to a vector file.
#' @export
aggregate_accessibility.gtfs_accessibility <- function(
  x,
  districts,
  ...
) {

  if (is.character(districts)) {

    if (!file.exists(districts)) {
      stop("Districts file does not exist.")
    }

    districts <- sf::st_read(
      districts,
      quiet = TRUE
    )
  }

  if (!inherits(districts, "sf")) {
    stop(
      "districts must be an sf object or a path to a vector file."
    )
  }

  if (!"district_name" %in% names(districts)) {
    stop("districts must contain a district_name column.")
  }

  districts <- sf::st_transform(
    districts,
    sf::st_crs(x$accessibility)
  )

  aggregated <- sf::st_join(
    districts,
    x$accessibility,
    left = FALSE
  )

  aggregated <- aggregated |>
    dplyr::group_by(.data$district_name) |>
    dplyr::summarise(
      mean_accessibility =
        mean(.data$accessibility_index, na.rm = TRUE),
      total_population =
        sum(.data$population_in_catchment, na.rm = TRUE),
      total_departures =
        sum(.data$departures, na.rm = TRUE),
      .groups = "drop"
    )

  new_gtfs_district_accessibility(
    accessibility = aggregated
  )
}


#' Classify Accessibility
#'
#' Classifies the continuous accessibility index of a `gtfs_accessibility`
#' object into a discrete, ordered factor (e.g. "Very Low" to "Very High"),
#' making it easier to summarise, map, and communicate accessibility
#' results. By default, classes are derived using Jenks natural breaks,
#' which minimise within-class variance and tend to suit the right-skewed
#' distributions accessibility indices typically have.
#'
#' @param x A gtfs_accessibility object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A gtfs_accessibility object with an additional
#'   accessibility_class column.
#'
#' @export
classify_accessibility <- function(x, ...) {
  UseMethod("classify_accessibility")
}

#' @rdname classify_accessibility
#' @param n_classes Number of accessibility classes. Default is 5.
#' @param method Classification method: "jenks", "quantile", or "equal".
#'
#' @examples
#' \dontrun{
#' accessibility <- classify_accessibility(accessibility)
#' table(accessibility$accessibility$accessibility_class)
#'
#' # Fewer, quantile-based classes
#' accessibility <- classify_accessibility(
#'   accessibility,
#'   n_classes = 3,
#'   method = "quantile"
#' )
#' }
#' @export
classify_accessibility.gtfs_accessibility <- function(
  x,
  n_classes = 5,
  method = c("jenks", "quantile", "equal"),
  ...
) {

  method <- match.arg(method)

  # n_classes must be a single whole number >= 2, or the classification
  # below either fails uninformatively or produces a meaningless result.
  if (!is.numeric(n_classes) || length(n_classes) != 1 ||
      n_classes < 2 || n_classes != round(n_classes)) {
    stop("n_classes must be a single whole number greater than or equal to 2.")
  }

  values <- x$accessibility$accessibility_index
  values <- values[!is.na(values)]

  if (length(unique(values)) < 2) {
    stop(
      "Accessibility values do not contain enough unique values for classification.",
      call. = FALSE
    )
  }

  breaks <- classInt::classIntervals(
    var = values,
    n = n_classes,
    style = method
  )$brks

  # Duplicate/tied values can collapse break points together; deduplicate
  # so cut() below doesn't error on non-unique breaks.
  breaks <- unique(breaks)

  labels <- switch(
    as.character(length(breaks) - 1),
    "3" = c("Low", "Moderate", "High"),
    "5" = c("Very Low", "Low", "Moderate", "High", "Very High"),
    paste("Class", seq_len(length(breaks) - 1))
  )

  x$accessibility$accessibility_class <- cut(
    x$accessibility$accessibility_index,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    ordered_result = TRUE
  )

  x
}


# -------------------------------------------
# HELPER FUNCTIONS
# -------------------------------------------

#' Extract Population by Catchment
#'
#' @param service_areas An sf object.
#' @param population A SpatRaster.
#'
#' @return The service areas with a population_in_catchment column.
#' @export
#' 
#' @examples
#' \dontrun{
#' population <- terra::rast("population.tif")
#'
#' service_areas <- create_service_areas(
#'   read_gtfs_stops("muenster_gtfs.zip"),
#'   "boundary.geojson"
#' )
#'
#' service_areas <- extract_population(
#'   service_areas,
#'   population
#' )
#'
#' head(service_areas)
#' }
extract_population <- function(
  service_areas,
  population
) {

  if (!inherits(service_areas, "sf")) {
    stop("service_areas must be an sf object.")
  }

  if (!inherits(population, "SpatRaster")) {
    stop("population must be a SpatRaster.")
  }

  service_areas <- sf::st_transform(
    service_areas,
    terra::crs(population)
  )

  values <- terra::extract(
    population,
    terra::vect(service_areas),
    fun = sum,
    na.rm = TRUE
  )

  service_areas$population_in_catchment <- values[, 2]

  service_areas
}

#' Count GTFS Departures
#'
#' Counts the total number of departures for each stop in a GTFS feed.
#'
#' @param gtfs_zip Path to a GTFS ZIP file.
#'
#' @return A data frame containing stop_id and departures.
#' @export
#' 
#' @examples
#' \dontrun{
#' departures <- count_gtfs_departures("muenster_gtfs.zip")
#'
#' head(departures)
#' }
count_gtfs_departures <- function(gtfs_zip) {

  if (!file.exists(gtfs_zip)) {
    stop("GTFS ZIP file does not exist.")
  }

  gtfs <- tidytransit::read_gtfs(gtfs_zip)

  if (is.null(gtfs$stop_times)) {
    stop("GTFS feed does not contain a stop_times table.")
  }

  stop_times <- gtfs$stop_times

  required_cols <- "stop_id"

  if (!all(required_cols %in% names(stop_times))) {
    stop(
      "stop_times must contain stop_id column."
    )
  }

  stop_times |>
    dplyr::count(
      .data$stop_id,
      name = "departures"
    )
}

#' Join Departures
#'
#' @param service_areas An sf object.
#' @param departures A data frame.
#'
#' @return Updated service areas.
#' @export
#' 
#' @examples
#' \dontrun{
#' departures <- count_gtfs_departures("muenster_gtfs.zip")
#'
#' service_areas <- create_service_areas(
#'   read_gtfs_stops("muenster_gtfs.zip"),
#'   "boundary.geojson"
#' )
#'
#' service_areas <- join_departures(
#'   service_areas,
#'   departures
#' )
#'
#' head(service_areas)
#' }
join_departures <- function(
  service_areas,
  departures
) {

  if (!inherits(service_areas, "sf")) {
    stop("service_areas must be an sf object.")
  }

  if (!is.data.frame(departures)) {
    stop("departures must be a data frame.")
  }

  required_cols <- c("stop_id", "departures")

  if (!all(required_cols %in% names(departures))) {
    stop("departures must contain stop_id and departures columns.")
  }

  service_areas <- merge(
    service_areas,
    departures,
    by = "stop_id",
    all.x = TRUE,
    sort = FALSE
  )

  service_areas$departures[
    is.na(service_areas$departures)
  ] <- 0

  service_areas
}