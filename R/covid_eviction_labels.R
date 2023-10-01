#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param label_data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param .labels PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[ggplot2]{geom_label}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{annotate}}, \code{\link[ggplot2]{geom_raster}}, \code{\link[ggplot2]{scale_manual}}, \code{\link[ggplot2]{guide_legend}}, \code{\link[ggplot2]{geom_abline}}
#'  \code{\link[lubridate]{ymd}}
#' @rdname covid_eviction_labels
#' @export 
#' @author Anthony Flores
#' @importFrom ggplot2 geom_label aes annotate geom_rect scale_fill_manual guide_legend geom_vline
#' @importFrom lubridate ymd
covid_eviction_labels <- function(label_data, ..., .labels = FALSE) {
  ojo_labels <- function(.label, data = label_data, .x, .y) {
    "Helper: Makes it more efficient to make and edit the aesthetics of all labels at once."
    ggplot2::geom_label(
      data = data,
      label = .label,
      ggplot2::aes(x = {{ .x }}, y = {{ .y }}, group = variable),
      fontface = "bold", size = 2.5, hjust = -0.02,
      label.size = 0, fill = "#dddddd", color = "#231f20"
    )
  }

  # Dates of events are rounded to nearest month for readability
  # COVID State of Emergency Declared
  date_covid <- lubridate::ymd("2020-03-01")
  # Federal Eviction Moratorium Begins (CARES Act passed 3-24-2020)
  date_federalMoratoriumBegins <- lubridate::ymd("2020-04-01")
  # Federal Eviction Moratorium Expires 7/25/2020
  date_federalMoratoriumExpires <- lubridate::ymd("2020-08-01")
  # CDC Moratorium Begins 9/4/2020
  date_cdcMoratoriumBegins <- lubridate::ymd("2020-09-01")
  # CDC Moratorium Expires 7/31/2021
  date_cdcMoratoriumEnds <- lubridate::ymd("2021-08-01")


  components <- list(
    # Highlight dates where there is an active eviction moratorium or court closure.
    ggplot2::annotate(
      "rect",
      xmin = date_covid, xmax = date_federalMoratoriumExpires,
      ymin = -Inf, ymax = Inf,
      fill = "#000000", alpha = 0.25,
      color = NA
    ),
    ggplot2::annotate(
      "rect",
      xmin = date_cdcMoratoriumBegins, xmax = date_cdcMoratoriumEnds,
      ymin = -Inf, ymax = Inf,
      fill = "#000000", alpha = 0.25,
      color = NA
    ),
    # We use this to make the legend (the x & y values don't matter)
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = date_covid, xmax = date_federalMoratoriumBegins,
        ymin = 0, ymax = Inf,
        fill = "Active Eviction Moratorium\nor Court Closure",
        color = NULL
      )
    ),
    ggplot2::scale_fill_manual(
      name = "",
      values = c("Active Eviction Moratorium\nor Court Closure" = "#00000000"),
      guide = ggplot2::guide_legend(override.aes = list(fill = "#000000", alpha = .2))
    )
  )


  if (.labels == TRUE) {
    components <- append(components, list(
      ggplot2::geom_vline(xintercept = date_covid, linetype = "dashed"),
      ggplot2::geom_vline(xintercept = date_federalMoratoriumBegins, linetype = "dashed"),
      ggplot2::geom_vline(xintercept = date_federalMoratoriumExpires, linetype = "dashed"),
      ggplot2::geom_vline(xintercept = date_cdcMoratoriumBegins, linetype = "dashed"),
      ggplot2::geom_vline(xintercept = date_cdcMoratoriumEnds, linetype = "dashed"),
      ojo_labels(
        data = label_data,
        .label = "COVID State of Emergency:\nCourt Closes Temporarily",
        .x = date_covid,
        .y = maxValue * .95
      ),
      ojo_labels(
        data = label_data,
        .label = "CARES Act: Federal Eviction Moratorium Issued",
        .x = date_federalMoratoriumBegins,
        .y = maxValue * .8
      ),
      ojo_labels(
        data = label_data,
        .label = "Federal Eviction\nMoratorium Expires",
        .x = date_federalMoratoriumExpires,
        .y = maxValue * .7
      ),
      ojo_labels(
        data = label_data,
        .label = "CDC Eviction\nMoratorium Issued",
        .x = date_cdcMoratoriumBegins,
        .y = maxValue * .2
      ),
      ojo_labels(
        data = label_data,
        .label = "CDC Eviction\nMoratorium Expires",
        .x = date_cdcMoratoriumEnds,
        .y = maxValue * .95
      )
    ))
  }


  return(components)
}
