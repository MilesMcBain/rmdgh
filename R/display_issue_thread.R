#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param issue_thread
#' @return
#' @author Miles McBain
#' @export
display_issue_thread <- function(issue_thread) {

  document_name <- snakecase::to_snake_case(
    glue::glue("{issue_thread$repo}_{issue_thread$number}")
  )

  create_temp_document(
      document_name,
      render_issue_thread_front_matter(issue_thread),
      paste(
        render_issue_thread(issue_thread$thread),"\n",
        render_issue_footer(),
        collapse = "\n"
      )
    ) |>
    rstudioapi::navigateToFile()

}
