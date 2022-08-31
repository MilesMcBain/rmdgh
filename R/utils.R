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

service_from_base_url <- function(url) {
  if (grepl("github.com", url, ignore.case = TRUE)) {
    "gh"
  }  else {
    stop("Unrecognised issue URL!")
  }
}

flatten_char <- function(char_vec) {
  if (is.null(char_vec)) return(NULL)
  paste0(char_vec, collapse = ", ")
}

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

api_issue_query <- function(repo, number) {
  glue::glue("/repos/{repo}/issues/{number}")
}