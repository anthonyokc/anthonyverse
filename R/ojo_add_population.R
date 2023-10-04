#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param geography PARAM_DESCRIPTION, Default: c("both", "county", "state")
#' @param year_column PARAM_DESCRIPTION
#' @param individual_year PARAM_DESCRIPTION, Default: NA
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
#'  \code{\link[dplyr]{select}}, \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{nth}}, \code{\link[dplyr]{pull}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{bind_rows}}, \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{join_by}}
#'  \code{\link[lubridate]{year}}, \code{\link[lubridate]{ymd}}
#'  \code{\link[tidycensus]{get_estimates}}
#'  \code{\link[tidyr]{pivot_wider}}
#' @rdname ojo_add_population
#' @export
#' @author Anthony Flores
#' @importFrom dplyr select filter starts_with nth pull tibble mutate bind_rows left_join join_by
#' @importFrom lubridate year ymd
#' @importFrom tidycensus get_estimates
#' @importFrom tidyr pivot_wider
ojo_add_population <- function(data,
                               geography = c("both", "county", "state"),
                               year_column,
                               individual_year = NA,
                               ...) {
  "Adds population data from the Census Population Estimate Program."

  # TODO: Add option for population density
  # TODO: Include a data source we host for when Census is down (maintenance, gov't shutdowns, etc.)

  ## Variable Handling
  # By default geography gets both the county and state
  .geography <- geography[1]

  # Add GEOIDs
  data <- data |>
    ojo_add_geoid(geography = "county", 2020)

  if (missing(year_column)) {
    # Find year columns in data, select first one in alphabetical order.
    year_column <- data |>
      dplyr::select(dplyr::starts_with("year")) |>
      names() |>
      sort() |>
      dplyr::nth(1)
  }

  years <- data |>
    dplyr::pull({{ year_column }}) |>
    unique() |>
    lubridate::year()

  get_population_data <- function(years, geography) {
    population_data <- dplyr::tibble()

    # TODO: Need to use censusapi to get the 2000s and earlier PEP estimates.
    # if (any(years %in% 2000:2009)) {
    #   population_2000s <- tidycensus::get_estimates(
    #     product = "population",
    #     geography = geography,
    #     year = 2009,
    #     time_series = TRUE,
    #     state = "OK"
    #   ) |>
    #   dplyr::filter(
    #     variable == "POP",
    #     !(DATE %in% c(1, 12)) # Excludes years 2010, and 2020 (https://www.census.gov/data/developers/data-sets/popest-popproj/popest/popest-vars/2019.html)
    #   ) |>
    #   dplyr::mutate(
    #     year = 2000 + (DATE - 2),
    #     variable = "POPESTIMATE"
    #   )
    #
    # population_data <- population_data |>
    #   dplyr::bind_rows(population_2000s)
    # }

    if (any(years %in% 2010:2019)) {
      population_2010s <- tidycensus::get_estimates(
        product = "population",
        geography = geography,
        year = 2019,
        time_series = TRUE,
        state = "OK"
      ) |>
        dplyr::filter(
          variable == "POP",
          !(DATE %in% c(1, 12))
        ) |>
        dplyr::mutate(
          year = 2010 + (DATE - 2),
          variable = "POPESTIMATE"
        )

      population_data <- population_data |>
        dplyr::bind_rows(population_2010s)
    }

    if (any(years %in% 2020:2029)) {
      population_2020s <- tidycensus::get_estimates(
        product = "population",
        geography = geography,
        time_series = TRUE,
        state = "OK"
      ) |>
        dplyr::filter(variable == "POPESTIMATE")

      population_data <- population_data |>
        dplyr::bind_rows(population_2020s)
    }

    population_data <- population_data |>
      tidyr::pivot_wider(names_from = "variable", values_from = "value") |>
      dplyr::mutate(year = paste0(year, "-01-01") |> lubridate::ymd()) |>
      dplyr::select(year, GEOID, population_county = POPESTIMATE)

    if (geography == "state") {
      population_data <- population_data |>
        dplyr::select(year, population_state = population_county)
    }

    return(population_data)
  }

  if (.geography %in% c("state", "both")) {
    population_state_data <- get_population_data(years, "state")
    data <- data |>
      dplyr::left_join(population_state_data, by = dplyr::join_by(!!year_column == year)) |>
      dplyr::mutate(population_state = as.numeric(population_state)) # as.numeric() is necessary for this to work.
  }
  if (.geography %in% c("county", "both")) {
    population_county_data <- get_population_data(years, "county")

    data <- data |>
      dplyr::left_join(population_county_data, by = dplyr::join_by(!!year_column == year, GEOID)) |>
      dplyr::mutate(population_county = as.numeric(population_county))
  }

  cat(
    paste("Joined population on the", paste0("`", year_column, "`"), "column."),
    sep = "\n"
  )

  return(data)
}
