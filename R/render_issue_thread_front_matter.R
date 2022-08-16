#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param issue_thread
#' @return
#' @author Miles McBain
#' @export
render_issue_thread_front_matter <- function(issue_thread) {

  front_matter_data <-
    list(
      title = issue_thread$title,
      author = issue_thread$author,
      output = list(`issuecreep::github_issue` = list(
        repo = issue_thread$repo,
        number = issue_thread$number,
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
