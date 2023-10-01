library(dplyr)
library(lubridate)
library(purrr)
ojo_add_month_and_year <- function(data, ...) {
  "Add month and year columns. Uses columns with 'date' by default but
  user can input the ones they want manually."

  cols <- rlang::quos(...)

  # If no specific columns provided, look for columns containing the word 'date'
  if (length(cols) == 0) {
    cols <- names(data)[grepl("date", names(data))]
  } else {
    cols <- purrr::map_chr(cols, ~ rlang::quo_text(.))
  }

  for (colname in cols) {
    month_name <- if (colname == "date") "month" else paste0("month_", gsub("date_", "", colname))
    year_name <- if (colname == "date") "year" else paste0("year_", gsub("date_", "", colname))

    data <- data %>%
      mutate(
        !!month_name := floor_date(!!sym(colname), "month"),
        !!year_name := floor_date(!!sym(colname), "year")
      )
  }

  return(data)
}
