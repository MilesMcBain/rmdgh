assert_github_exists <- function(repo, issue = NULL) {
  gh_issue <- glue::glue("/issues/{issue}")
  gh_repo <- glue::glue("/repos/{repo}")
  query <- paste0(gh_repo, gh_issue, collapse = "")
  tryCatch(
    gh::gh(
      query
    ),
    error = function(e) {
      stop("could not find repository on GitHub: ", repo, ".\n{gh} said:\n", e$message)
    }
  )
  invisible(TRUE)
}

assert_CRAN_page_exists <- function(repo) {
  # I tried to make this a HEAD request but the CRAN server doesn't seem to respond to
  # HEAD correctly.
  res <- curl::curl_fetch_memory(cran_url(repo))

  if (res$status_code != 200) stop(repo, " is not installed locally, and could not be located on CRAN")
}

assert_is_rmd <- function(document_context) {
  document_extension <- fs::path_ext(document_context$path)
  assertthat::assert_that(document_extension %in% c("Rmd", "Qmd"))
}

assert_github_issue <- function(issue_yaml) {
  assertthat::assert_that(
    assertthat::has_name(issue_yaml, "output")
  )
  assertthat::assert_that(
    assertthat::has_name(issue_yaml$output, "issuecreep::github_issue")
  )
}