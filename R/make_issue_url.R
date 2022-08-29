make_issue_url <- function(reference) UseMethod("make_issue_url", reference)

#' @export
make_issue_url.shortcode <- function(shortcode) {
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

  switch(
    service,
    gh = glue::glue("https://github.com/{repo}/issues/{issue_number}"),
    stop("unknown shortcode service: ", service)
  )

}

#' @export
make_issue_url.hashref <- function(hashref) {

  issue_number <- regmatches(
    hashref,
    regexpr("[0-9]+", hashref)
  )
  remote_url <- gert::git_remote_info()$url
  service <- regmatches(
    remote_url,
    regexpr("[A-Za-z0-9]+\\.com", remote_url)
  )
  repo <- get_repo_remote()
  
  assert_github_exists(repo, issue = issue_number)
  
  glue::glue(
    "https://github.com/{repo}/issues/{issue_number}"
  )
  

}
