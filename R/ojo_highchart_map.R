library(ggthemes)
library(jsonlite)
library(geojsonsf)
library(highcharter)
ojo_highchart_map <- function(data,
                              variable,
                              ...,
                              caption = NULL,
                              nbins,
                              map_color_stops = rev(viridis::magma(n = 3))
                              ) {

  variable <- enquo(variable) |> rlang::as_label()


  data_json <- data |>
    select(district) |>
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
    pull({{variable}}) |>
    na.omit()

  n_bins <- nclass.FD(variable_data)
  if (!missing(nbins)) {
    n_bins <- nbins

    if (is.function(nbins)) {
      bin_formula <- nbins
      n_bins <- bin_formula(variable_data)
    }
  }

  h <- hist(variable_data, plot = FALSE, breaks = n_bins)

  d <- diff(h$breaks)[1]

  df <- tibble(
        x = h$mids,
        y = h$counts,
        name = sprintf("(%s, %s]", h$mids - d / 2, h$mids + d / 2)
    )

  legend_colors <- color_classes(
          breaks = h$breaks,
          colors =
            color_stops(
              n = n_bins,
              colors = map_color_stops
            ) |>
            sapply(function(x) x[[2]])
  )

 hc <- highchart() |>
    hc_add_series_map(
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
    hc_caption(text = caption) |>
    hc_colorAxis(
        labels = list(
          format = "{value}",
          formatter = highchart_label_formatter(variable)
        ),
        dataClasses = legend_colors
    ) |>
    hc_legend(
        layout = "vertical",
        align = "left",
        verticalAlign = "top",
        floating = TRUE,
        backgroundColor = 'rgba(255,255,255,0.9)',
        valueDecimals = 2,
        labelFormatter = highchart_label_formatter(variable, dataClass = TRUE),
        symbolRadius = 0,
        itemMarginBottom = 4,
        itemMarginTop = 4,
        shadow = TRUE
    ) |>
    hc_mapNavigation(
        enabled = TRUE,
        buttonOptions = list(
          align = "right"
        )
    ) |>
    hc_add_theme(hc_theme_ojo)

  return(hc)
}
