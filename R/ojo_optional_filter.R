#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param variable PARAM_DESCRIPTION
#' @param variable_values PARAM_DESCRIPTION, Default: NA
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
#' @rdname ojo_optional_filter
#' @export 
#' @author Anthony Flores
#' @importFrom dplyr filter
ojo_optional_filter <- function(data, variable, variable_values = NA) {
  "Helper for optional filtering of variable if not NA."

  if (any(!is.na(variable_values))) {
    data <- data |> dplyr::filter({{ variable }} %in% variable_values)
  }

  return(data)
}
