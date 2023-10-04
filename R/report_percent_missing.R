#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param column PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{nth}}, \code{\link[dplyr]{pull}}
#' @rdname report_percent_missing
#' @export
#' @author Anthony Flores
#' @importFrom dplyr filter nth pull
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
