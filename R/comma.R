#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param digits PARAM_DESCRIPTION, Default: 0
#' @param abs PARAM_DESCRIPTION, Default: FALSE
#' @param round_to_zero PARAM_DESCRIPTION, Default: FALSE
#' @param drop0trailing PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[janitor]{round_half_up}}
#' @rdname comma
#' @export
#' @author Anthony Flores
#' @importFrom janitor round_half_up
comma <- function(x,
                  digits = 0,
                  abs = FALSE,
                  round_to_zero = FALSE,
                  drop0trailing = FALSE
                  ) {
  .drop0trailing <- drop0trailing
  .digits <-  digits


  # Ensures that a value like 0.4 will be displayed as 0.4 and not be rounded to zero.
  if (round_to_zero == FALSE) {
    if (.digits == 0 & janitor::round_half_up(x, 0) == 0) {
        .digits <- 1
        x <- janitor::round_half_up(x, .digits)
    }
  } else {
    x <- janitor::round_half_up(x, digits = .digits)
  }

  # Take absolute value of x before formatting
  if (abs) {
    x <- abs(x)
  }
  x <-
    formatC(
      x,
      big.mark = ",",
      format = "f",
      digits = .digits,
      drop0trailing = .drop0trailing
    )
  return(x)

}
