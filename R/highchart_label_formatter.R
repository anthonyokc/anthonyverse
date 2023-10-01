#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param variable_name PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION, Default: ''
#' @param suffix PARAM_DESCRIPTION, Default: ''
#' @param value_identifier PARAM_DESCRIPTION, Default: 'this.value'
#' @param digits PARAM_DESCRIPTION, Default: 0
#' @param dataClass PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_detect}}, \code{\link[stringr]{str_replace}}
#'  \code{\link[htmlwidgets]{JS}}
#' @rdname highchart_label_formatter
#' @export 
#' @author Anthony Flores
#' @importFrom stringr str_detect str_replace_all
#' @importFrom htmlwidgets JS
highchart_label_formatter <- function(variable_name,
                                      prefix = "",
                                      suffix = "",
                                      value_identifier = "this.value",
                                      digits = 0,
                                      dataClass = FALSE) {
  if (stringr::str_detect(variable_name, "[P|p]ercent")) {
    formatted_value <- "Highcharts.numberFormat(this.value * 100, 0) + '%'"
  } else if (stringr::str_detect(variable_name, "[I|i]ncome|[A|a]mount")) {
    formatted_value <- "'$' + Highcharts.numberFormat(this.value, 0)"
  } else {
    formatted_value <- "Highcharts.numberFormat(this.value, 0)"
  }

  if (value_identifier != "this.value") {
    formatted_value <-
      stringr::str_replace_all(formatted_value, "this.value", value_identifier)
  }

  if (digits != 0) {
    formatted_value <-
      stringr::str_replace_all(formatted_value, "0\\)", paste0(digits, "\\)"))
  }

  if (dataClass == TRUE) {
    modified_from <- stringr::str_replace_all(formatted_value, "this.value", "this.from")
    modified_to <- stringr::str_replace_all(formatted_value, "this.value", "this.to")

    return(htmlwidgets::JS(paste0(
      "function() {
                return ", prefix,
      modified_from, " +  ' — '  + ", modified_to,
      suffix, ";", "
            }"
    )))
  }

  return(htmlwidgets::JS(paste0("function() {
            return ", prefix, formatted_value, suffix, ";", "
        }")))
}
