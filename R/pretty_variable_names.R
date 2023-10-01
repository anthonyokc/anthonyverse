pretty_variable_names <- function(variable_names) {

    pretty_names <- variable_names |>
        snakecase::to_title_case() |>
        str_replace_all(
            pattern = "Percent Population",
            replacement = "% of Population,"
        ) |>
        str_replace_all("Nhpi", "NHPI") |>
        str_replace_all("Non Hispanic", "Non-Hispanic") |>
        str_replace_all("^N\\s", "Number of ") |>
        str_replace_all("\\sN\\s", "Number of ")

    return(pretty_names)
}
