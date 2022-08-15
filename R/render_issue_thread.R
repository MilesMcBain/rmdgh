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

  lapply(thread, function(comment) {
    render_thread_comment(comment)
  }) %>%
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
