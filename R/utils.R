gh_url <- function(path) {
  glue::glue("https://github.com/{path}")
}


gh_api_url <- function(repo, issue_number) {
  glue::glue(
    "https://api.github.com/repos/{repo}/issues/{issue_number}"
  )
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
  } else {
    stop("Unrecognised issue URL!")
  }
}

flatten_char <- function(char_vec) {
  if (is.null(char_vec)) {
    return(NULL)
  }
  paste0(char_vec, collapse = ", ")
}

`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

api_issue_query <- function(repo, number) {
  glue::glue("/repos/{repo}/issues/{number}")
}

is_gh_url <- function(input) {
  if (length(input) > 1 && !is.character(input)) {
    return(FALSE)
  }
  grepl("^(https|http)://", input) &&
    grepl("github.com", input)
}

extract_issue_info_from_gh_url <- function(gh_url) {
  result <- regmatches(
    gh_url,
    regexec(
      "github.com/(?P<owner>[A-Za-z.-_]+)/(?P<repo>[A-Za-z.-_]+)/(?P<type>issues|pulls)/(?P<number>[0-9]+)",
      gh_url,
      perl = TRUE
    )
  )[[1]]

  repo <-
    paste(result["owner"], result["repo"], sep = "/")
  number <-
    unname(result["number"])

  structure(
    list(
      repo = repo,
      number = number,
      api_url = gh_api_url(repo, number)
    ),
    class = "issue_info"
  )
}

is_issue_shorthand <- function(text) {
  if (length(text) > 1 && !is.character(text)) {
    return(FALSE)
  }
  grepl("[A-Za-z.-_]+/[A-Za-z.-_]+#[0-9]+", text) |
    grepl("[A-Za-z.-_]+#[0-9]+", text)
}

extract_issue_info_from_shorthand <- function(text) {
  result <- regmatches(
    text,
    regexec(
      "(?P<owner>[A-Za-z.-_]+)/(?P<repo>[A-Za-z.-_]+)#(?P<number>[0-9]+)",
      text,
      perl = TRUE
    )
  )[[1]]

  if (is.na(result["repo"])) {
    result <- regmatches(
      text,
      regexec(
        "(?P<repo>[A-Za-z.-_]+)#(?P<number>[0-9]+)",
        text,
        perl = TRUE
      )
    )[[1]]

    if (is.na(result["repo"]) | is.na(result["number"])) {
      stop("Valid shorthand syntax is <repo>#<number> or <owner>/<repo>#number")
    }

    repo <- resolve_repo(result["repo"])
  } else {
    repo <-
      paste(result["owner"], result["repo"], sep = "/")
  }

  number <-
    unname(result["number"])

  structure(
    list(
      repo = repo,
      number = number,
      api_url = gh_api_url(repo, number)
    ),
    class = "issue_info"
  )
}
