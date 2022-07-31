display_issue_search_results <- function(result) {
  stop("Not implemented yet")
  if (!rstudioapi::isAvailable()) stop("not handled yet!")

  
  browser()
  create_temp_document(
    package_name(package),
    render_issue_search_results(issues)
  ) |>
  rstudioapi::navigateToFile()
  
}