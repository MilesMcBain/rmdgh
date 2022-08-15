#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param result
#' @return
#' @author Miles McBain
#' @export
render_issue_search_front_matter <- function(result) {
  paste0(
    "---\n",
    yaml::as.yaml(result[c(
      "query",
      "cache_key",
      "current_page",
      "max_page"
    )]),
    "---\n"
  )
}
