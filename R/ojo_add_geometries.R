#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param geography PARAM_DESCRIPTION, Default: c("county", "state")
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
#'  \code{\link[tigris]{counties}}, \code{\link[tigris]{states}}
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{cross_join}}
#'  \code{\link[stats]{filter}}
#' @rdname ojo_add_geometries
#' @export 
#' @author Anthony Flores
#' @importFrom tigris counties states
#' @importFrom dplyr select left_join cross_join
#' @importFrom stats filter
ojo_add_geometries <- function(data, geography = c("county", "state"), ...) {
  geography <- geography[1]

  if (geography == "county") {
    data <- tigris::counties("OK", progress_bar = FALSE, ...) |>
      dplyr::select(GEOID) |>
      dplyr::left_join(data)

    return(data)
  }

  if (geography == "state") {
    data <- tigris::states(progress_bar = FALSE, ...) |>
      stats::filter(NAME == "Oklahoma") |>
      dplyr::select(geometry) |>
      dplyr::cross_join(data)
  }

  return(data)
}
