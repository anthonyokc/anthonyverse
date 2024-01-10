#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param group_by PARAM_DESCRIPTION
#' @param geography PARAM_DESCRIPTION, Default: c("county", "state")
#' @param rate PARAM_DESCRIPTION, Default: 10^3
#' @param rate_suffix PARAM_DESCRIPTION, Default: '1k'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}}, \code{\link[rlang]{quosure-tools}}, \code{\link[rlang]{as_label}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{if_else}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{arrange}}
#' @rdname ojo_aggregate
#' @export
#' @author Anthony Flores
#' @importFrom rlang enquos quo_get_expr as_label
#' @importFrom purrr map_chr
#' @importFrom dplyr if_else distinct count mutate arrange
ojo_aggregate <- function(data,
                          ...,
                          group_by,
                          geography = c("county", "state"),
                          rate = 10^3,
                          rate_suffix = "1k") {
  # Variable Handling
  geography <- geography[1]
  variables <- rlang::enquos(...)
  if (!missing(group_by)) {
    group_by_text <- rlang::enquos(group_by) |> purrr::map_chr(~ rlang::quo_get_expr(.) |> rlang::as_label())
  }

  if (length(variables) == 0) {
    variable_name <- NA
  } else {
    variable_name <- purrr::map_chr(variables, ~ rlang::as_label(.))
  }

  n_variable_name <- dplyr::if_else(
    is.na(variable_name),
    "n",
    paste0("n_", variable_name) # E.g., n_eviction_filings
  )
  rate_variable_name <- dplyr::if_else(
    is.na(variable_name),
    paste0("n_", "per_", rate_suffix),
    paste0("n_", variable_name, "_per_", rate_suffix) # E.g., n_eviction_filings_per1k
  )

  data <- data |>
    slice_tail(n = 1, by = id)

  if (any(is.na(variable_name))) {
    variable_name <- ""

    if (geography == "county") {
      data <- data |>
        dplyr::count({{ group_by }}, district, population_county, name = n_variable_name)
    } else if (geography == "state") {
      data <- data |>
        dplyr::count({{ group_by }}, population_state, name = n_variable_name)

      data <- data |>
        dplyr::mutate(
          !!rate_variable_name := .data[[n_variable_name]] / population_state * rate
        ) |>
        dplyr::arrange({{ group_by }}, !!variable_name)

      return(data)
    }
  } else {
    if (geography == "county") {
      data <- data |>
        dplyr::count(
          {{ group_by }}, district, population_county, .data[[variable_name]],
          name = n_variable_name
        )
    } else if (geography == "state") {
      data <- data |>
        dplyr::count(
          {{ group_by }}, district, population_county, .data[[variable_name]],
          name = n_variable_name
        )

      data <- data |>
        dplyr::mutate(
          !!rate_variable_name := .data[[n_variable_name]] / population_state * rate
        ) |>
        dplyr::arrange({{ group_by }}, !!variable_name)

      return(data)
    }
  }

  data <- data |>
    dplyr::mutate(!!rate_variable_name := .data[[n_variable_name]] / population_county * rate) |>
    dplyr::arrange({{ group_by }}, district, !!variable_name)

  return(data)
}
