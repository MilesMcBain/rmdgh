#' @export
extract_issues <- function(result, ...) UseMethod("extract_issues", result)

#' @export
extract_issues.default <- function(result) {
  stop(
    "Can't extract issues from object with classes: ",
    paste(class(result), collapse = ", ")
  )
}

#' @author Miles McBain
#' @export
extract_issues.gh_response <- function(result) {

  lapply(result$items, function(item) {
    list(
      number = item$number,
      title = item$title,
      body = item$body,
      repo = get_repo_from_url(item$html_url),
      api_url = item$url,
      html_url = item$html_url,
      labels = extract_labels_gh(item$labels),
      author = item$user$login,
      created_at = item$created_at
    )
  })

}

extract_labels_gh <- function(item_labels) {
  lapply(item_labels, function(label) label$name) %>%
    unlist()
}
