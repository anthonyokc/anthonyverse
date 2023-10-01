ojo_optional_filter <- function(data, variable, variable_values = NA) {
    "Helper for optional filtering of variable if not NA."

    if (any(!is.na(variable_values))) {
      data <-  data |> filter({{variable}} %in% variable_values)
    }

    return(data)
}
