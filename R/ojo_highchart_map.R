#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param variable PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param caption PARAM_DESCRIPTION, Default: NULL
#' @param nbins PARAM_DESCRIPTION
#' @param map_color_stops PARAM_DESCRIPTION, Default: rev(viridis::magma(n = 3))
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[viridis]{reexports}}
#'  \code{\link[rlang]{enquo}}, \code{\link[rlang]{as_label}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{reexports}}
#'  \code{\link[geojsonsf]{sf_geojson}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[stats]{na.fail}}
#'  \code{\link[grDevices]{nclass}}
#'  \code{\link[graphics]{hist}}
#'  \code{\link[highcharter]{color_classes}}, \code{\link[highcharter]{color_stops}}, \code{\link[highcharter]{highchart}}, \code{\link[highcharter]{hc_add_series_map}}, \code{\link[highcharter]{hc_caption}}, \code{\link[highcharter]{hc_colorAxis}}, \code{\link[highcharter]{hc_legend}}, \code{\link[highcharter]{hc_mapNavigation}}, \code{\link[highcharter]{hc_add_theme}}
#' @rdname ojo_highchart_map
#' @export 
#' @author Anthony Flores
#' @importFrom viridis magma
#' @importFrom rlang enquo as_label
#' @importFrom dplyr select pull tibble
#' @importFrom geojsonsf sf_geojson
#' @importFrom jsonlite fromJSON
#' @importFrom stats na.omit
#' @importFrom grDevices nclass.FD
#' @importFrom graphics hist
#' @importFrom highcharter color_classes color_stops highchart hc_add_series_map hc_caption hc_colorAxis hc_legend hc_mapNavigation hc_add_theme
ojo_highchart_map <- function(data,
                              variable,
                              ...,
                              caption = NULL,
                              nbins,
                              map_color_stops = rev(viridis::magma(n = 3))) {
  variable <- rlang::enquo(variable) |> rlang::as_label()


  data_json <- data |>
    dplyr::select(district) |>
    geojsonsf::sf_geojson() |>
    jsonlite::fromJSON(simplifyVector = FALSE)

  # Highlight around specific areas
  # data_hc <-
  #   data |>
  #   left_join(
  #     tibble(
  #       district = __,
  #       borderColor = ojo_pal[1],
  #       borderWidth = 3,
  #       zIndex = 5000,
  #       candidate = 1
  #     ),
  #     by = "district"
  #   )

  # variable_data <- data_hc |>
  variable_data <- data |>
    dplyr::pull({{ variable }}) |>
    stats::na.omit()

  n_bins <- grDevices::nclass.FD(variable_data)
  if (!missing(nbins)) {
    n_bins <- nbins

    if (is.function(nbins)) {
      bin_formula <- nbins
      n_bins <- bin_formula(variable_data)
    }
  }

  h <- graphics::hist(variable_data, plot = FALSE, breaks = n_bins)

  d <- diff(h$breaks)[1]

  df <- dplyr::tibble(
    x = h$mids,
    y = h$counts,
    name = sprintf("(%s, %s]", h$mids - d / 2, h$mids + d / 2)
  )

  legend_colors <- highcharter::color_classes(
    breaks = h$breaks,
    colors =
      highcharter::color_stops(
        n = n_bins,
        colors = map_color_stops
      ) |>
        sapply(function(x) x[[2]])
  )

  hc <- highcharter::highchart() |>
    highcharter::hc_add_series_map(
      map = data_json,
      df = data,
      joinBy = "district",
      id = "s1",
      name = pretty_variable_names(variable),
      value = variable,
      tooltip = list(
        headerFormat = "{point.name}<br>",
        pointFormatter = highchart_label_formatter(
          variable,
          prefix = "'<b>' + this.district + '</b><br>' + this.series.name + ': <b>' + ",
          suffix = " + '</b><br>'",
          # "+ 'Margin of Error:,
          # paste0("<b>this.moe_", variable),
          # "&plusmn</b>'",
          digits = 1
        )
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.district}"
      )
    ) |>
    highcharter::hc_caption(text = caption) |>
    highcharter::hc_colorAxis(
      labels = list(
        format = "{value}",
        formatter = highchart_label_formatter(variable)
      ),
      dataClasses = legend_colors
    ) |>
    highcharter::hc_legend(
      layout = "vertical",
      align = "left",
      verticalAlign = "top",
      floating = TRUE,
      backgroundColor = "rgba(255,255,255,0.9)",
      valueDecimals = 2,
      labelFormatter = highchart_label_formatter(variable, dataClass = TRUE),
      symbolRadius = 0,
      itemMarginBottom = 4,
      itemMarginTop = 4,
      shadow = TRUE
    ) |>
    highcharter::hc_mapNavigation(
      enabled = TRUE,
      buttonOptions = list(
        align = "right"
      )
    ) |>
    highcharter::hc_add_theme(hc_theme_ojo)

  return(hc)
}
