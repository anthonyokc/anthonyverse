library(tigris)
library(sf)
ojo_add_geometries <- function(data, geography = c("county", "state"), ...) {
  geography <- geography[1]

  if (geography == "county") {
    data <- counties("OK", progress_bar = FALSE, ...) |>
      select(GEOID) |>
      left_join(data)

    return(data)
  }

  if (geography == "state") {
    data <- states(progress_bar = FALSE, ...) |>
      filter(NAME == "Oklahoma") |>
      select(geometry) |>
      cross_join(data)
  }

  return(data)
}
