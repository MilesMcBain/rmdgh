#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param issue_thread
#' @return
#' @author Miles McBain
#' @export
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
