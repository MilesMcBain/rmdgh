#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param matching_issue
#' @return
#' @author Miles McBain
#' @export
get_issue_thread <- function(matching_issue) {

  result <- gh::gh(
    glue::glue("/repos/{matching_issue$repo}/issues/{matching_issue$number}/comments")
  )

  initial_comment <- extract_comments(
    matching_issue
  )
  comments <- extract_comments(
    result
  )

  thread <-
    c(
      initial_comment,
      comments
    )
  thread
}
