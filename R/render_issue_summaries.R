render_issue_summaries <- function(issue_items) {
  vapply(
    issue_items,
    render_issue_summary,
    character(1),
    USE.NAMES = FALSE
  ) |>
  paste0(
    collapse = "\r\n"
  )
}

render_issue_summary <- function(issue_json) {
  issue_content <-
    c(
      issue_title(issue_json$title, issue_json$number),
      issue_meta_summary(
        issue_json$user$login,
        issue_json$created_at,
        issue_json$comments,
        issue_json$reactions$total_count),
      empty_line(),
      issue_body_summary(issue_json$body),
      empty_line(),
      issue_url(issue_json$html_url),
      horizontal_rule(),
      empty_line()
    )

    paste0(
      issue_content,
      collapse = "\r\n"
    )
}

issue_title <- function(title, number) {
  glue::glue("# {title} ({number})")
}

issue_meta_summary <- function(author, created_at, n_comments, n_reactions) {
  glue::glue(
    "Created {time_diff(created_at)}",
    "by @{author}",
    "{n_comments} comments",
    "{n_reactions} reactions",
    .sep = " "
    )
}

issue_body_summary <- function(issue_body) {
  body_lines <- strsplit(issue_body, "\r\n")[[1]]
  body_lines_head <- head(body_lines, 3)
  paste0(strrep(" ", 4), body_lines_head) # makes it quoted
}

empty_line <- function() ""

issue_url <- function(issue_url) issue_url

horizontal_rule <- function() strrep("-", 80)