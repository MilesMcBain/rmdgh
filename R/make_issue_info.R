make_issue_info <- function(reference, ...) UseMethod("make_issue_info", reference)

#' @export
make_issue_info.shortcode <- function(shortcode, ...) {
  service <- regmatches(
    shortcode,
    regexpr("(?<=`)[a-z]{2}", shortcode, perl = TRUE)
  )

  repo <- regmatches(
    shortcode,
    regexpr("(?<=\\s)[A-Za-z0-9_./-]+", shortcode, perl = TRUE)
  )

  issue_number <- regmatches(
    shortcode,
    regexpr("(?<=#)[0-9]+", shortcode, perl = TRUE)
  )

  html_url <- switch(
    service,
    gh = glue::glue("https://github.com/{repo}/issues/{issue_number}"),
    stop("unknown shortcode service: ", service)
  )

  structure(
    list(
      repo = repo,
      number = issue_number,
      html_url = html_url
    ),
    class = "issue_info"
  )
}

#' @export
make_issue_info.hashref <- function(hashref, document_context) {

  issue_number <- regmatches(
    hashref,
    regexpr("[0-9]+", hashref)
  )
  assert_is_rmd(document_context)
  doc_yaml <- rmarkdown::yaml_front_matter(document_context$path)
  repo <- doc_yaml$output$`issuecreep::github_issue`$repo %||% get_repo_remote()
  
  assert_github_exists(repo, issue = issue_number)
  
  html_url <- glue::glue(
    "https://github.com/{repo}/issues/{issue_number}"
  )

  structure(
    list(
      repo = repo,
      number = issue_number,
      html_url = html_url
    ),
    class = "issue_info"
  )
}
