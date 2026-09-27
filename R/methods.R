#' Print a GTFS Stops Object
#'
#' @param x A gtfs_stops object.
#' @param ... Additional arguments.
#'
#' @export
print.gtfs_stops <- function(x, ...) {

  cat("GTFS Stops Object\n")
  cat("-----------------\n")
  cat("Number of stops:", nrow(x$stops), "\n")

  invisible(x)
}

#' Summarize a GTFS Stops Object
#'
#' @param object A gtfs_stops object.
#' @param ... Additional arguments.
#'
#' @export
summary.gtfs_stops <- function(object, ...) {

  cat("GTFS Stops Summary\n")
  cat("------------------\n")
  cat("Number of stops:", nrow(object$stops), "\n")
  cat("Geometry type:", unique(sf::st_geometry_type(object$stops)), "\n")
  cat("CRS:", sf::st_crs(object$stops)$input, "\n")

  invisible(object)
}


#' Plot GTFS Stops
#'
#' @param x A gtfs_stops object.
#' @param pch Point symbol.
#' @param col Point colour.
#' @param cex Point size.
#' @param ... Additional graphical arguments.
#'
#' @export
plot.gtfs_stops <- function(
  x,
  pch = 16,
  col = "red",
  cex = 0.7,
  ...
) {

  plot(
    sf::st_geometry(x$stops),
    pch = pch,
    col = col,
    main = "GTFS Stops",
    ...
  )

  invisible(x)
}

#' Print a GTFS Accessibility Object
#'
#' @param x A gtfs_accessibility object.
#' @param ... Additional arguments.
#'
#' @export
print.gtfs_accessibility <- function(x, ...) {

  mean_index <- mean(
    x$accessibility$accessibility_index,
    na.rm = TRUE
  )

  cat("GTFS Accessibility Object\n")
  cat("-------------------------\n")
  cat(
    "Number of catchments:",
    nrow(x$accessibility),
    "\n"
  )
  cat(
    "Mean accessibility:",
    round(mean_index, 3),
    "\n"
  )

  invisible(x)
}


#' Summarize a GTFS Accessibility Object
#'
#' @param object A gtfs_accessibility object.
#' @param ... Additional arguments.
#'
#' @export
summary.gtfs_accessibility <- function(object, ...) {

  stats <- accessibility_statistics(object)

  cat("GTFS Accessibility Summary\n")
  cat("--------------------------\n")

  cat(
    "Number of catchments:",
    nrow(object$accessibility),
    "\n"
  )

  cat(
    "Mean accessibility:",
    round(stats$mean_accessibility, 3),
    "\n"
  )

  cat(
    "Median accessibility:",
    round(stats$median_accessibility, 3),
    "\n"
  )

  cat(
    "Maximum accessibility:",
    round(stats$max_accessibility, 3),
    "\n"
  )

  cat(
    "Minimum accessibility:",
    round(stats$min_accessibility, 3),
    "\n"
  )

  invisible(object)
}


#' Plot GTFS Accessibility
#'
#' Plots each catchment shaded by its accessibility index. By default
#' (`classified = FALSE`) catchments are shaded on a continuous scale,
#' classed into quantile bins so the colour spans the distribution of
#' values rather than being dominated by a few extreme outliers. With
#' `classified = TRUE`, catchments are instead grouped into named,
#' ordered categories (e.g. "Very Low" to "Very High") using whatever
#' classification was set by \code{\link{classify_accessibility}}; if
#' the object hasn't been classified yet, a default 5-class Jenks
#' classification is computed on the fly purely for the plot.
#'
#' @param x A gtfs_accessibility object.
#' @param classified Logical. If `TRUE`, plot named accessibility
#'   classes instead of the continuous index. Default is `FALSE`.
#' @param n_classes Integer. Number of quantile classes to use for the
#'   continuous view (`classified = FALSE`). Ignored when
#'   `classified = TRUE`. Default is 6.
#' @param ... Additional graphical arguments passed to plot().
#'
#' @export
plot.gtfs_accessibility <- function(
  x,
  classified = FALSE,
  n_classes = 6,
  ...
) {

  if (classified) {

    plot_classified_choropleth(
      geometry = sf::st_geometry(x$accessibility),
      values = x$accessibility$accessibility_index,
      class_factor = x$accessibility$accessibility_class,
      title = "GTFS Accessibility Classes",
      legend_title = "Accessibility\nclass",
      ...
    )

  } else {

    plot_quantile_choropleth(
      geometry = sf::st_geometry(x$accessibility),
      values = x$accessibility$accessibility_index,
      n_classes = n_classes,
      title = "GTFS Accessibility Index (raw)",
      legend_title = "Departures per\n 100 resident",
      ...
    )

  }

  invisible(x)
}


#' Print a GTFS District Accessibility Object
#'
#' @param x A gtfs_district_accessibility object.
#' @param ... Additional arguments.
#'
#' @export
print.gtfs_district_accessibility <- function(x, ...) {

  mean_index <- mean(
    x$accessibility$mean_accessibility,
    na.rm = TRUE
  )

  cat("GTFS District Accessibility Object\n")
  cat("-----------------------------------\n")
  cat("Number of districts:", nrow(x$accessibility), "\n")
  cat("Mean accessibility across districts:", round(mean_index, 3), "\n")

  invisible(x)
}


#' Plot GTFS District Accessibility
#'
#' Plots each district shaded by its mean accessibility index, using
#' the same raw/classified logic as \code{\link{plot.gtfs_accessibility}}.
#'
#' @param x A gtfs_district_accessibility object.
#' @param classified Logical. If `TRUE`, plot named accessibility
#'   classes instead of the continuous mean index. Default is `FALSE`.
#' @param n_classes Integer. Number of quantile classes to use for the
#'   continuous view (`classified = FALSE`). Ignored when
#'   `classified = TRUE`. Default is 6.
#' @param ... Additional graphical arguments passed to plot().
#'
#' @export
plot.gtfs_district_accessibility <- function(
  x,
  classified = FALSE,
  n_classes = 6,
  ...
) {

  if (classified) {

    plot_classified_choropleth(
      geometry = sf::st_geometry(x$accessibility),
      values = x$accessibility$mean_accessibility,
      class_factor = NULL,
      title = "District Accessibility Classes",
      legend_title = "Accessibility\nclass",
      ...
    )

  } else {

    plot_quantile_choropleth(
      geometry = sf::st_geometry(x$accessibility),
      values = x$accessibility$mean_accessibility,
      n_classes = n_classes,
      title = "District Accessibility (raw index)",
      legend_title = "Mean accessibility\nindex",
      ...
    )

  }

  invisible(x)
}


# -------------------------------------------
# INTERNAL PLOTTING HELPERS
# (shared by plot.gtfs_accessibility and plot.gtfs_district_accessibility)
# -------------------------------------------

#' Format Class Break Numbers for Legends
#'
#' Formats a vector of numeric class breaks into readable
#' "low - high" interval labels, avoiding scientific notation
#' and adding thousands separators for large values.
#'
#' @keywords internal
#'
#' @param breaks A numeric vector of class break points, sorted ascending.
#'
#' @return A character vector of interval labels, one shorter than `breaks`.
format_breaks <- function(breaks) {

  rounded <- round(breaks, 1)

  format_one <- function(v) {
    format(v, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  vapply(
    seq_len(length(rounded) - 1),
    function(i) {
      paste0(format_one(rounded[i]), " - ", format_one(rounded[i + 1]))
    },
    character(1)
  )
}

#' Plot a Continuous Choropleth with Quantile Classing
#'
#' Shared implementation behind the raw (`classified = FALSE`) view of
#' both `plot.gtfs_accessibility()` and `plot.gtfs_district_accessibility()`.
#' Quantile classing is used instead of equal-width bins because
#' accessibility values are typically right-skewed: equal-width bins
#' would let a handful of extreme values dominate the colour scale.
#'
#' @keywords internal
#'
#' @param geometry An sfc geometry column to plot.
#' @param values A numeric vector, one value per geometry.
#' @param n_classes Integer number of quantile classes.
#' @param title Plot title.
#' @param legend_title Legend title.
#' @param ... Additional graphical arguments passed to plot().
#'
#' @return Invisibly, NULL.
plot_quantile_choropleth <- function(
  geometry,
  values,
  n_classes,
  title,
  legend_title,
  ...
) {

  palette <- grDevices::hcl.colors(n_classes, "Viridis")

  if (length(unique(stats::na.omit(values))) > 1) {

    breaks <- classInt::classIntervals(
      stats::na.omit(values),
      n = n_classes,
      style = "quantile"
    )$brks

    # Duplicate/tied quantile edges can collapse breaks together
    breaks <- unique(breaks)

    bins <- cut(values, breaks = breaks, include.lowest = TRUE)
    cols <- palette[bins]
    legend_labels <- format_breaks(breaks)
    legend_fill <- palette[seq_len(length(levels(bins)))]

  } else {

    bins <- NULL
    cols <- rep(palette[length(palette)], length(values))

  }

  plot(
    geometry,
    col = cols,
    border = "grey85",
    lwd = 0.3,
    main = title,
    ...
  )

  if (!is.null(bins)) {

    graphics::legend(
      "bottomright",
      legend = legend_labels,
      fill = legend_fill,
      cex = 0.55,
      title = legend_title,
      bty = "n"
    )

  }

  invisible(NULL)
}

#' Plot a Categorical (Classified) Choropleth
#'
#' Shared implementation behind the `classified = TRUE` view of both
#' `plot.gtfs_accessibility()` and `plot.gtfs_district_accessibility()`.
#' If `class_factor` is supplied (e.g. the `accessibility_class` column
#' set by \code{\link{classify_accessibility}}), it is used directly;
#' otherwise a default 5-class Jenks classification is computed from
#' `values` on the fly, purely for this plot.
#'
#' @keywords internal
#'
#' @param geometry An sfc geometry column to plot.
#' @param values A numeric vector, one value per geometry. Only used
#'   when `class_factor` is `NULL`.
#' @param class_factor An existing (ordered) factor of class labels,
#'   or `NULL` to classify `values` automatically.
#' @param title Plot title.
#' @param legend_title Legend title.
#' @param ... Additional graphical arguments passed to plot().
#'
#' @return Invisibly, NULL.
plot_classified_choropleth <- function(
  geometry,
  values,
  class_factor,
  title,
  legend_title,
  ...
) {

  if (is.null(class_factor)) {

    complete_values <- stats::na.omit(values)

    if (length(unique(complete_values)) < 2) {
      stop("Not enough distinct values to classify for plotting.")
    }

    breaks <- classInt::classIntervals(
      complete_values,
      n = 5,
      style = "jenks"
    )$brks
    breaks <- unique(breaks)

    n <- length(breaks) - 1
    labels <- if (n == 5) {
      c("Very Low", "Low", "Moderate", "High", "Very High")
    } else {
      paste("Class", seq_len(n))
    }

    class_factor <- cut(
      values,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      ordered_result = TRUE
    )

  }

  class_levels <- levels(class_factor)

  palette <- stats::setNames(
    grDevices::hcl.colors(length(class_levels), "Viridis"),
    class_levels
  )

  plot(
    geometry,
    col = palette[as.character(class_factor)],
    border = "grey85",
    lwd = 0.3,
    main = title,
    ...
  )

  graphics::legend(
    "bottomright",
    legend = names(palette),
    fill = palette,
    cex = 0.6,
    title = legend_title,
    bty = "n"
  )

  invisible(NULL)
}