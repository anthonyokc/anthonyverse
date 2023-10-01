#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param color_variable PARAM_DESCRIPTION, Default: variable
#' @param facet_variable PARAM_DESCRIPTION, Default: NA
#' @param legend_labels PARAM_DESCRIPTION, Default: NA
#' @param facet_labels PARAM_DESCRIPTION, Default: NA
#' @param custom_color_palette PARAM_DESCRIPTION, Default: NA
#' @param number_of_columns PARAM_DESCRIPTION, Default: 1
#' @param data_to_highlight PARAM_DESCRIPTION, Default: NA
#' @param highlight_color PARAM_DESCRIPTION, Default: 'red'
#' @param theme PARAM_DESCRIPTION, Default: custom_ojo_theme()
#' @param add_covid_labels PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_flatten}}
#'  \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{summarise}}
#'  \code{\link[tidyr]{pivot_longer}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{geom_path}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{facet_wrap}}, \code{\link[ggplot2]{vars}}, \code{\link[ggplot2]{as_labeller}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{margin}}, \code{\link[ggplot2]{scale_continuous}}, \code{\link[ggplot2]{scale_manual}}, \code{\link[ggplot2]{c("guide_bins", "guide_colourbar", "guide_coloursteps", "guide_legend", "guides", "guides")}}, \code{\link[ggplot2]{guide_legend}}
#'  \code{\link[scales]{label_number}}
#' @rdname ojo_plot_lines
#' @export 
#' @author Anthony Flores
#' @importFrom stringr str_flatten
#' @importFrom dplyr if_else select matches distinct pull any_of summarise
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap vars as_labeller theme element_blank scale_y_continuous scale_color_manual guides guide_legend
#' @importFrom scales label_comma
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
                           add_covid_labels = FALSE) {
  # Create strings from inputs
  x_string <- deparse(substitute(x))
  if (is.symbol(substitute(y)) | is.language(substitute(y))) {
    y_string <- deparse(substitute(y))
  } else if (is.language(substitute(y))) {
    y_string <- sapply(substitute(y)[-1], function(x) deparse(x))
    y_string <- stringr::str_flatten(y_string)
  } else {
    y_string <- y
  }
  color_variable_string <- deparse(substitute(color_variable))
  facet_variable_string <- deparse(substitute(facet_variable))
  if (color_variable_string == "variable") {
    color_variable_string <- ""
  }

  # Regex selection of the x, y, and color_variable columns.
  regex_columns <- paste0(
    x_string,
    "|",
    dplyr::if_else(
      color_variable_string == "",
      "",
      paste0(color_variable_string, "|")
    ),
    y_string
  )

  plotData <-
    data |>
    dplyr::select(dplyr::matches(regex_columns))

  # Construct wide data to input to ggplot based on the color_variable input
  if (color_variable_string == "") {
    plotData <-
      plotData |>
      tidyr::pivot_longer(!{{ x }}, names_to = "variable", values_to = "value")

    p <-
      plotData |>
      ggplot2::ggplot(ggplot2::aes(x = {{ x }}, y = value, color = variable))

    number_of_variables <-
      plotData |>
      dplyr::distinct(variable) |>
      dplyr::pull() |>
      length()
  } else {
    plotData <-
      plotData |>
      tidyr::pivot_longer(
        !dplyr::any_of(c(x_string, color_variable_string)),
        names_to = "variable", values_to = "value"
      )

    p <-
      plotData |>
      ggplot2::ggplot(ggplot2::aes(x = {{ x }}, y = value, color = {{ color_variable }}))

    number_of_variables <-
      plotData |>
      dplyr::distinct({{ color_variable }}) |>
      dplyr::pull() |>
      length()
  }

  # Format labels for use in ggplot
  if (any(!is.na(legend_labels))) {
    legend_variables <- plotData |>
      dplyr::distinct(variable) |>
      dplyr::pull()
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
      dplyr::summarise(maxValue = max(value), .by = variable)
    p <- p +
      covid_eviction_labels(label_data = labelData)
  }

  p <- p +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::geom_point(shape = 19, size = 2)

  # Add optional highlighting of certain data points.
  if (is.object(data_to_highlight)) {
    data_to_highlight <-
      data_to_highlight |>
      dplyr::select(dplyr::matches(regex_columns)) |>
      tidyr::pivot_longer(
        !dplyr::any_of(c(x_string, color_variable_string)),
        names_to = "variable", values_to = "value"
      )

    p <- p +
      ggplot2::geom_point(data = data_to_highlight, shape = 19, size = 2, color = highlight_color)
  }

  p <- p +
    ggplot2::geom_point(shape = 21, size = 2, stroke = 1, color = "black")

  if (facet_variable_string != "NA") {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars({{ facet_variable }}),
        ncol = number_of_columns, scales = "free",
        labeller = ggplot2::as_labeller(facet_labels)
      )
  }

  if (any(!is.na(theme))) {
    p <- p +
      theme
  }

  p <- p +
    ggplot2::theme(
      axis.title = ggplot2::element_blank()
    )

  if (any(is.na(facet_labels))) {
    p <- p +
      ggplot2::theme(
        strip.text = ggplot2::element_blank()
      )
  }

  p <- p +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::scale_color_manual(values = ojo_pal[1:number_of_variables]) +
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(
        shape = rep(19, number_of_variables),
        linetype = 0,
        size = 3,
        fill = NA
        )
    ))

  # Customize color pallette
  if (any(!is.na(custom_color_palette))) {
    p <- p +
      ggplot2::scale_color_manual(values = custom_color_palette[1:number_of_variables])
  }

  # Add custom labels for legend
  if (any(!is.na(legend_labels))) {
    p <- p +
      ggplot2::scale_color_manual(
        values = ojo_pal[1:number_of_variables],
        labels = legend_labels
      )
  }

  return(p)
}
