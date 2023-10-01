ojo_aggregate <- function(data,
                          ...,
                          group_by,
                          geography = c("county", "state"),
                          rate = 10^3,
                          rate_suffix = "1k"
                          ) {

    # Variable Handling
    geography <- geography[1]
    variables <- enquos(...)
    if (!missing(group_by)) {
      group_by_text <- enquos(group_by) |> purrr::map_chr(~ rlang::quo_get_expr(.) |> rlang::as_label())
    }

    if (length(variables) == 0) {
      variable_name <- NA
    } else {
      variable_name <- purrr::map_chr(variables, ~ rlang::as_label(.))
    }

    n_variable_name <- if_else(
      is.na(variable_name),
      "n",
      paste0("n_", variable_name) # E.g., n_eviction_filings
    )
    rate_variable_name <- if_else(
      is.na(variable_name),
      paste0("n_", "per_", rate_suffix),
      paste0("n_", variable_name, "_per_", rate_suffix) # E.g., n_eviction_filings_per1k
    )

    data <- data |>
      distinct(id, .keep_all = TRUE)

    if (any(is.na(variable_name))) {
        variable_name <- ""

        if (geography == "county") {

          data <- data |>
            count({{group_by}}, district, population_county, name = n_variable_name)

        } else if (geography == "state") {

          data <- data |>
            count({{group_by}}, population_state, name = n_variable_name)

            data <- data |>
                mutate(
                  !!rate_variable_name := .data[[n_variable_name]] / population_state * rate
                ) |>
                arrange({{group_by}}, !!variable_name)

            return(data)

        }
    } else {
        if (geography == "county") {

          data <- data |>
            count(
              {{group_by}}, district, population_county, .data[[variable_name]],
              name = n_variable_name
            )

        } else if (geography == "state") {

          data <- data |>
            count(
              {{group_by}}, district, population_county, .data[[variable_name]],
              name = n_variable_name
            )

          data <- data |>
              mutate(
                !!rate_variable_name := .data[[n_variable_name]] / population_state * rate
              ) |>
              arrange({{group_by}}, !!variable_name)

          return(data)
        }
    }

    data <- data |>
        mutate(!!rate_variable_name := .data[[n_variable_name]] / population_county * rate) |>
        arrange({{group_by}}, district, !!variable_name)

    return(data)
}
