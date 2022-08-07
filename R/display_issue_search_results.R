display_issue_search_results <- function(result) {

  document_name <- snakecase::to_snake_case(
    paste(
      result$query_description,
      result$query$package,
      collapse = " "
    )
  )

  create_temp_document(
    document_name,
    render_issue_search_results(result$issues)
  ) |>
    rstudioapi::navigateToFile()

}