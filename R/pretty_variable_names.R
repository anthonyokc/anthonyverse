#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param variable_names PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[snakecase]{caseconverter}}
#'  \code{\link[stringr]{str_replace}}
#' @rdname pretty_variable_names
#' @export 
#' @author Anthony Flores
#' @importFrom snakecase to_title_case
#' @importFrom stringr str_replace_all
pretty_variable_names <- function(variable_names) {
  pretty_names <- variable_names |>
    snakecase::to_title_case() |>
    stringr::str_replace_all(
      pattern = "Percent Population",
      replacement = "% of Population,"
    ) |>
    stringr::str_replace_all("Nhpi", "NHPI") |>
    stringr::str_replace_all("Non Hispanic", "Non-Hispanic") |>
    stringr::str_replace_all("^N\\s", "Number of ") |>
    stringr::str_replace_all("\\sN\\s", "Number of ")

  return(pretty_names)
}
