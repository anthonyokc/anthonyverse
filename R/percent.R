#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param digits PARAM_DESCRIPTION, Default: 0
#' @param abs PARAM_DESCRIPTION, Default: FALSE
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
#' @rdname percent
#' @export
#' @author Anthony Flores
#' @importFrom janitor round_half_up
percent <- function(x,
                    digits = 0,
                    abs = FALSE,
                    drop0trailing = FALSE,
                    multiply_by_100 = TRUE,
                    ...
                    ) {

  .drop0trailing <- drop0trailing
  .digits <- digits

  if (abs) { x <- abs(x) }
  if (multiply_by_100) { x <- 100 * x }

  x <- janitor::round_half_up(x, digits = .digits)
  x <-
    formatC(
        x,
        big.mark = ",",
        format = "f",
        digits = .digits,
        drop0trailing = .drop0trailing
    ) |>
    paste0("%")

  return(x)
}
