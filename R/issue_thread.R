issue_thread_filename <- function(issue_thread) {
  snakecase::to_snake_case(
    glue::glue("{issue_thread$repo}_{issue_thread$number}")
  )
}

render_issue_thread_front_matter <- function(issue_thread) {
  front_matter_data <-
    list(
      title = issue_thread$title,
      author = issue_thread$author,
      output = list(`rmdgh::github_issue` = list(
        repo = issue_thread$repo,
        number = issue_thread$number,
        labels = issue_thread$labels,
        action = "comment",
        draft = TRUE,
        close_with_comment = FALSE
      ))
    )

  paste0(
    "---\n",
    yaml::as.yaml(front_matter_data),
    "---\n"
  )

}
render_issue_thread <- function(thread) {

  issue_body <-
    render_thread_comment(thread[[1]])

  issue_comments <-
    lapply(thread[-1], function(comment) {
      render_thread_comment(comment)
    })

  c(
    issue_body,
    render_issue_body_footer(),
    issue_comments
  ) %>%
    glue::glue_collapse(sep = "\n")

}

render_thread_comment <- function(comment) {

  info <- glue::glue(
    "*@{comment$author} wrote {time_diff(comment$created_at)}*"
  )

  glue::glue(
    "{info}{strrep('-', max(80 - nchar(info), 3))}\n",
    "{comment$body}\n",
    .trim = FALSE
  )

}

render_issue_thread <- function(thread) {

  issue_body <-
    render_thread_comment(thread[[1]])

  issue_comments <-
    lapply(thread[-1], function(comment) {
      render_thread_comment(comment)
    })

  c(
    issue_body,
    render_issue_body_footer(),
    issue_comments
  ) %>%
    glue::glue_collapse(sep = "\n")

}

render_thread_comment <- function(comment) {

  info <- glue::glue(
    "*@{comment$author} wrote {time_diff(comment$created_at)}*"
  )

  glue::glue(
    "{info}{strrep('-', max(80 - nchar(info), 3))}\n",
    "{comment$body}\n",
    .trim = FALSE
  )

}
render_issue_footer <- function() {
  "<!-- %Type your comment below. Content on and above line will be removed.% -->"
}

render_issue_thread_content <- function(issue_thread) {
  paste(
    render_issue_thread(issue_thread$thread),
    "\n",
    render_issue_footer(),
    collapse = "\n",
    sep = ""
  )
}

render_issue_thread_document <- function(issue_thread) {
  paste0(
    render_issue_thread_front_matter(issue_thread),
    "\n",
    render_issue_thread_content(issue_thread)
  )
}


display_issue_thread <- function(issue_thread) {

  document_name <- issue_thread_filename(issue_thread)

  create_temp_document(
    document_name,
    render_issue_thread_front_matter(issue_thread),
    render_issue_thread_content(issue_thread)
  ) |>
    rstudioapi::navigateToFile()

}

replace_issue_thread <- function(issue_thread, document_context) {

  rstudioapi::modifyRange(
    rstudioapi::document_range(
      rstudioapi::document_position(1, 0),
      rstudioapi::document_position(length(document_context$contents), Inf)
    ),
    render_issue_thread_document(issue_thread),
    id = document_context$id
  )

}

construct_issue_thread <- function(
  issue,
  issue_comments
) {

}