library(ojodb)
library(ggplot2)

ojo_plot_lines <- function(data,
                      x,
                      y,
                      ...,
                      color_variable = variable,
                      facet_variable = NA,
                      # legend_labels = "default",
                      # facet_labels = "default",
                      legend_labels = NA,
                      facet_labels = NA,
                      custom_color_palette = NA,
                      number_of_columns = 1,
                      data_to_highlight = NA,
                      highlight_color = "red",
                      theme = custom_ojo_theme(),
                      add_covid_labels = FALSE
) {


  # Create strings from inputs
  x_string <- deparse(substitute(x))
  if (is.symbol(substitute(y)) | is.language(substitute(y))) {
    y_string <- deparse(substitute(y))
  } else if (is.language(substitute(y))) {
    y_string <- sapply(substitute(y)[-1], function(x) deparse(x))
    y_string <- str_flatten(y_string)
  } else {
    y_string <- y
  }
  color_variable_string <- deparse(substitute(color_variable))
  facet_variable_string <- deparse(substitute(facet_variable))
  if (color_variable_string == "variable") {
    color_variable_string <- ""
  }

  # Regex selection of the x, y, and color_variable columns.
  regex_columns <-paste0(
    x_string,
    "|",
    if_else(
      color_variable_string == "",
      "",
      paste0(color_variable_string, "|")
    ),
    y_string
  )

  plotData <-
    data |>
    select(matches(regex_columns))

  # Construct wide data to input to ggplot based on the color_variable input
  if (color_variable_string == "") {
    plotData <-
      plotData |>
      pivot_longer(!{{x}}, names_to = "variable", values_to = "value")

    p <-
      plotData |>
      ggplot(aes(x = {{x}}, y = value, color = variable))

    number_of_variables <-
      plotData |>
      distinct(variable) |>
      pull() |>
      length()
  } else {
    plotData <-
      plotData |>
      pivot_longer(
        !any_of(c(x_string, color_variable_string)),
        names_to = "variable", values_to = "value"
      )

    p <-
      plotData |>
      ggplot(aes(x = {{x}}, y = value, color = {{color_variable}}))

    number_of_variables <-
      plotData |>
      distinct({{color_variable}}) |>
      pull() |>
      length()
  }

  # Format labels for use in ggplot
  if (any(!is.na(legend_labels))) {
    legend_variables <- plotData |> distinct(variable) |> pull()
    names(legend_labels) <- legend_variables
  }
  # if (any(!is.na(facet_labels))) {
  #     facets <- plotData |> distinct({{facet_variable}}) |> pull()
  #     names(facet_labels) <- facets
  # }

  # Optional labels for covid eviction moratoriums.
  if (add_covid_labels == TRUE) {
    labelData <-
      plotData |>
      summarise(maxValue = max(value), .by = variable)
    p <- p +
      covid_eviction_labels(label_data = labelData)
  }

  p <- p +
    geom_line(linewidth = 1.5) +
    geom_point(shape = 19, size = 2)

  # Add optional highlighting of certain data points.
  if (is.object(data_to_highlight)) {
    data_to_highlight <-
      data_to_highlight |>
      select(matches(regex_columns)) |>
      pivot_longer(
        !any_of(c(x_string, color_variable_string)),
        names_to = "variable", values_to = "value"
      )

    p <- p +
      geom_point(data = data_to_highlight, shape = 19, size = 2, color = highlight_color)
  }

  p <- p +
    geom_point(shape = 21, size = 2, stroke = 1, color = "black")

  if (facet_variable_string != "NA") {
    p <- p +
      facet_wrap(
        vars({{facet_variable}}), ncol = number_of_columns, scales = "free",
        labeller = as_labeller(facet_labels)
      )
  }

  if (any(!is.na(theme))) {
    p <- p +
      theme
  }

  p <- p +
    theme(
      axis.title = element_blank()
    )

  if (any(is.na(facet_labels)))
    p <- p +
    theme(
      strip.text = element_blank()
    )

  p <- p +
    scale_y_continuous(labels = scales::label_comma()) +
    scale_color_manual(values = ojo_pal[1:number_of_variables]) +
    guides(color = guide_legend(override.aes = list(
      shape = rep(19, number_of_variables), linetype = 0, size = 3, fill = NA
    ))
    )

  # Customize color pallette
  if (any(!is.na(custom_color_palette))) {
    p <- p +
      scale_color_manual(values = custom_color_palette[1:number_of_variables])
  }

  # Add custom labels for legend
  if (any(!is.na(legend_labels))) {
    p <- p +
      scale_color_manual(
        values = ojo_pal[1:number_of_variables],
        labels = legend_labels
      )
  }

  return(p)
}
