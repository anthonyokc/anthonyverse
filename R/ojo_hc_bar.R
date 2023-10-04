ojo_hc_bar <- function(hc, data, variable, breaks = "FD", ...) {

  if (!missing(variable)) {
    data <- data |>
      pull({{variable}})

    variable_name <- pretty_variable_names(
      variable |> enquo() |> rlang::quo_get_expr() |> as.character()
    )
  }

  h <- hist(data, plot = FALSE, breaks = breaks)

  n_bins <- length(h$breaks) - 1

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
        colors = viridis::magma(n = 3) |> rev()
      ) |>
      sapply(function(x) x[[2]])
  )

  hc |>  hc_tooltip(formatter = JS("
              function() { return  this.point.name + '<br/>' + this.y; }
        ")) |>
    htmlwidgets::prependContent(tags$style(htmltools::HTML("
          rect.highcharts-point {
            stroke: #000000;
            stroke-width: 1px
          }
        "))) |>
    hc_add_series(
      data = list_parse(df),
      type = "column",
      name = variable_name,
      # color = "gold",
      colorByPoint = TRUE,
      colors = color_stops(
        n = n_bins,
        colors = viridis::magma(n = 3) |> rev()
      ) |>
        sapply(function(x) x[[2]]),
      pointRange = d,
      groupPadding = 0,
      pointPadding = 0,
      borderWidth = 0,
      ...
    ) |>
    # hc_colorAxis(
    #     labels = list(
    #       format = "{value}",
    #       formatter = highchart_label_formatter(variable_name)
    #     ),
    #     dataClasses = legend_colors
    # ) |>
    hc_xAxis(
      labels = list(
        formatter = highchart_label_formatter(variable_name)
      )
    ) |>
    hc_add_theme(hc_theme_ojo())
}
