#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param geography PARAM_DESCRIPTION, Default: c("county", "state")
#' @param year PARAM_DESCRIPTION, Default: 2019
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{is_installed}}
#'  \code{\link[tidycensus]{get_estimates}}
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{mutate-joins}}
#'  \code{\link[stringr]{str_remove}}, \code{\link[stringr]{case}}, \code{\link[stringr]{str_replace}}
#' @rdname ojo_add_geoid
#' @export 
#' @author Anthony Flores
#' @importFrom rlang check_installed
#' @importFrom tidycensus get_estimates
#' @importFrom dplyr mutate select left_join
#' @importFrom stringr str_remove str_to_upper str_replace_all
ojo_add_geoid <- function(data,
                          geography = c("county", "state"),
                          year = 2019) {
  rlang::check_installed(pkg = "tidycensus")

  # Variable Handling
  .geography <- geography[1]

  population_variable <- "POPESTIMATE"

  if (year <= 2019) {
    population_variable <- "POP"
  }

  df <- tidycensus::get_estimates(
    variables = population_variable,
    geography = .geography,
    year = year,
    state = "OK"
  )

  if (.geography == "county") {
    df <-
      df |>
      dplyr::mutate(
        district =
          NAME |>
            stringr::str_remove(" County.*") |>
            stringr::str_to_upper() |>
            stringr::str_replace_all("LE FLORE", "LEFLORE")
      ) |>
      dplyr::select(district, GEOID)

    data <-
      data |>
      dplyr::left_join(df)

    return(data)
  }

  data <-
    data |>
    dplyr::mutate(GEOID = df$GEOID)

  return(data)
}
