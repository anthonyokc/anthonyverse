report_percent_missing <- function(data, column) {
  na_data <- data |>
    tably({{column}}) |>
    dplyr::filter(is.na({{column}}))

  print(na_data)

  na_column_name <- na_data |>
    names() |>
    dplyr::nth(1)

  na_data |>
    dplyr::pull(percent) |>
    percent(multiply_by_100 = FALSE, 2) |>
    sprintf(fmt = "\n%s of '%s' are missing.", na_column_name) |>
    cat()
}
