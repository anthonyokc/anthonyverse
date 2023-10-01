library(dplyr)
library(lubridate)
library(purrr)
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{defusing-advanced}}, \code{\link[rlang]{quo_label}}, \code{\link[rlang]{sym}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[lubridate]{round_date}}
#' @rdname ojo_add_month_and_year
#' @export 
#' @author Anthony Flores
#' @importFrom rlang quos quo_text sym
#' @importFrom purrr map_chr
#' @importFrom dplyr mutate
#' @importFrom lubridate floor_date
ojo_add_month_and_year <- function(data, ...) {
  "Add month and year columns. Uses columns with 'date' by default but
  user can input the ones they want manually."

  cols <- rlang::quos(...)

  # If no specific columns provided, look for columns containing the word 'date'
  if (length(cols) == 0) {
    cols <- names(data)[grepl("date", names(data))]
  } else {
    cols <- purrr::map_chr(cols, ~ rlang::quo_text(.))
  }

  for (colname in cols) {
    month_name <- if (colname == "date") "month" else paste0("month_", gsub("date_", "", colname))
    year_name <- if (colname == "date") "year" else paste0("year_", gsub("date_", "", colname))

    data <- data %>%
      dplyr::mutate(
        !!month_name := lubridate::floor_date(!!rlang::sym(colname), "month"),
        !!year_name := lubridate::floor_date(!!rlang::sym(colname), "year")
      )
  }

  return(data)
}
