issue_search_results_forward <- function() {
  active_doc <- rstudioapi::getSourceEditorContext()

  doc_yaml <- yaml::read_yaml(active_doc$path)
  search_cache_key <- doc_yaml$cache_key

  issue_search_results <- get_cached_result(search_cache_key)
  if (issue_search_results$current_page == issue_search_results$max_page) {
    message("No more results for this issue search. (max/min page reached)")
    return()
  }

  new_result_object <- gh::gh_next(issue_search_results$result_obj)

  make_search_result(
    issues = extract_issues(new_result_object),
    result_obj = new_result_object,
    query = issue_search_results$query,
    query_description = issue_search_results$query_description,
    current_page = issue_search_results$current_page + 1,
    max_page = issue_search_results$max_page,
    cache_key = issue_search_results$cache_key
  ) %>%
  update_cached_result(key = issue_search_results$cache_key) %>%
  display_issue_search_results()
  
}

issue_search_results_backward <- function() {}

issue_search_results_expand <- function() {}

issue_search_results_collapse <- function() {}