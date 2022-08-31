#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param document_context
#' @param issue_info
#' @return
#' @author Miles McBain
#' @export
get_maybe_cached_issue <- function(document_context, issue_info) {

  doc_yaml <- rmarkdown::yaml_front_matter(document_context$path)

  get_issue_from_cached_search_results(doc_yaml, issue_info) %||%
    get_issue_from_issue_info(issue_info)
}

get_issue_from_cached_search_results <- function(doc_yaml, issue_info) {

  if (length(doc_yaml) == 0) return(NULL)

  search_cache_key <- doc_yaml$cache_key
  if (is.null(search_cache_key)) return(NULL)

  issue_search_results <- get_cached_result(search_cache_key)
  if (is.null(issue_search_results)) return(NULL)

  search_result_urls <- vapply(
    issue_search_results$issues,
    function(issue) issue$api_url,
    character(1)
  )

  matching_issue <-
    issue_search_results$issues[search_result_urls == issue_info$api_url][[1]]

  matching_issue
}

get_issue_from_issue_info <- function(issue_info) {
  extract_issues(gh::gh(
    api_issue_query(issue_info$repo, issue_info$number)
  ))
}