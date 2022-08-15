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
  c(
    issue_thread[c("title", "author","repo", "number")],
    list(
      draft = TRUE,
      close_with_comment = FALSE
    )
  )

  paste0(
    "---\n",
    yaml::as.yaml(front_matter_data),
    "---\n"
  )

}
