#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param print_n PARAM_DESCRIPTION, Default: NA
#' @param top_n PARAM_DESCRIPTION, Default: NA
#' @param sort PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{enquo}}, \code{\link[rlang]{sym}}
#'  \code{\link[dplyr]{count}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{slice}}, \code{\link[dplyr]{summarise}}
#' @rdname tably
#' @export 
#' @author Anthony Flores
#' @importFrom rlang enquos sym
#' @importFrom dplyr count mutate slice summarise
tably <- function(data, ..., print_n = NA, top_n = NA, sort = TRUE) {
  # TODO: add check for x is missing.
  # TODO: add check for using print_n & top_n at the same time.

  # Format arguments
  .sort <- sort
  dots <- rlang::enquos(...)

  df <-
    data |>
    dplyr::count(!!!dots, sort = .sort) |>
    dplyr::mutate(
      percent = n / sum(n) * 100,
      total_n = sum(n)
    )

  # if you want to the output to print more than 10.
  if (!is.na(print_n)) {
    return(
      df |>
        print(n = print_n)
    )
  }

  # Returns the frequency and percent frequency of the top n observations
  # and of the observations not in the top n.
  if (!is.na(top_n)) {
    df <-
      df |>
      dplyr::slice(1:top_n) |>
      dplyr::summarise(!!paste0("top", top_n, "_n") := sum(n),
        !!paste0("top", top_n, "_percent") := sum(percent),
        .by = total_n
      ) |>
      dplyr::mutate(
        !!paste0("non_top", top_n, "_n") :=
          total_n - !!rlang::sym(paste0("top", top_n, "_n")),
        !!paste0("non_top", top_n, "_percent") :=
          100 - !!rlang::sym(paste0("top", top_n, "_percent"))
      )
  }

  return(df)
}
