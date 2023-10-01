ojo_district_and_date_filters <- function(data,
                                          districts = "ALL",
                                          date_start = NA,
                                          date_end = NA,
                                          district_variable = "district",
                                          date_variable = "date_filed"
                                          ) {

    "Helper function for commonly used case filters."

    # Variable Handling
    .districts <- toupper(districts)

    if (any(districts != "ALL")) {
        data <-  data |> filter(.data[[district_variable]] %in% .districts)
    }
    if (!is.na(date_end)) {
        data <- data |> filter(.data[[date_variable]] <= date_end)
    }
    if (!is.na(date_start)) {
        data <-  data |> filter(.data[[date_variable]] >= date_start)
    }

    return(data)
}
