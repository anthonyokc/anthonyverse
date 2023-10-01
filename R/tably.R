tably <- function (data, ..., print_n = NA, top_n = NA, sort = TRUE) {
  # TODO: add check for x is missing.
  # TODO: add check for using print_n & top_n at the same time.

  # Format arguments
  .sort <- sort
  dots <- enquos(...)

  df <-
    data |>
    count(!!!dots, sort = .sort) |>
    mutate(percent = n / sum(n) * 100,
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
    slice(1:top_n) |>
    summarise(!!paste0("top", top_n, "_n") := sum(n),
              !!paste0("top", top_n, "_percent") := sum(percent),
              .by = total_n
              ) |>
    mutate(!!paste0("non_top", top_n, "_n") :=
             total_n - !!sym(paste0("top", top_n, "_n")),
           !!paste0("non_top", top_n, "_percent") :=
             100 - !!sym(paste0("top", top_n, "_percent"))
           )
  }

  return(df)
}
