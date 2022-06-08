
issue_content <- function(content) {
  downlevel_issue_headers(content)
}

downlevel_issue_headers <- function(issue_content) {
  content_data_frame <- parse_issue_content(issue_content)
  downleveled_content <-
    within(
      content_data_frame,
      line[is_header] <- paste0("#", line[is_header])
    )

  paste0(
    downleveled_content$line,
    collapse = "\r\n"
  )
}