gh_url <- function(path) {
  glue::glue("https://github.com/{path}")
}

lead_indicator <- function(indicator_vec) {
  c(
    indicator_vec[-1],
    FALSE
  )
}

lag_indicator <- function(indicator_vec) {
  c(
    FALSE,
    indicator_vec[-length(indicator_vec)]
  )
}

time_diff <- function(timestamp) {
  prettyunits::vague_dt(
    lubridate::now() - lubridate::as_datetime(timestamp)
  )
}
