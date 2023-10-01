ojo_add_geoid <- function(data,
                          geography = c("county", "state"),
                          year = 2019
                          ) {

    rlang::check_installed(pkg = "tidycensus")

    # Variable Handling
    .geography = geography[1]

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
                    str_replace_all("LE FLORE", "LEFLORE")
            ) |>
            dplyr::select(district, GEOID)

        data <-
            data |>
            dplyr::left_join(df)

        return(data)
    }

    data <-
        data |>
        mutate(GEOID = df$GEOID)

    return(data)
}
