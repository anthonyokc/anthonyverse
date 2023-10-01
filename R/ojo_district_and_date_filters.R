#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param districts PARAM_DESCRIPTION, Default: 'ALL'
#' @param date_start PARAM_DESCRIPTION, Default: NA
#' @param date_end PARAM_DESCRIPTION, Default: NA
#' @param district_variable PARAM_DESCRIPTION, Default: 'district'
#' @param date_variable PARAM_DESCRIPTION, Default: 'date_filed'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}}
#' @rdname ojo_district_and_date_filters
#' @export 
#' @author Anthony Flores
#' @importFrom dplyr filter
ojo_district_and_date_filters <- function(data,
                                          districts = "ALL",
                                          date_start = NA,
                                          date_end = NA,
                                          district_variable = "district",
                                          date_variable = "date_filed") {
  "Helper function for commonly used case filters."

  # Variable Handling
  .districts <- toupper(districts)

  if (any(districts != "ALL")) {
    data <- data |> dplyr::filter(.data[[district_variable]] %in% .districts)
  }
  if (!is.na(date_end)) {
    data <- data |> dplyr::filter(.data[[date_variable]] <= date_end)
  }
  if (!is.na(date_start)) {
    data <- data |> dplyr::filter(.data[[date_variable]] >= date_start)
  }

  return(data)
}
