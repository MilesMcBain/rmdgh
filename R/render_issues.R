render_issue <- function(issue_json) {
  issue_content <-
    c(
      issue_title(issue_json$title, issue_json$number),
      issue_content(issue_json$body),
      empty_line(),
      issue_url(issue_json$html_url),
      horizontal_rule(),
      empty_line()
    )

}

issue_title <- function(title, number) {
  glue::glue("# {title} ({number})")
}

issue_content <- function(content) {
  downlevel_issue_headers(content)
}

downlevel_issue_headers <- function(issue_content) {
 # TODO 
}
