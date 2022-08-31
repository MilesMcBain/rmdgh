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

  if ("items" %in% names(result)) {
    lapply(result$items, function(item) {
      make_issue(item)
    })
  } else if (
    all(c(
      "html_url",
      "number",
      "title",
      "body",
      "labels"
    ) %in% names(result))
  ) {
    make_issue(result)
  } else {
    stop(
      "Can't extract issue(s) from {gh} API result, unknown type of result."
    )
  }

}

extract_labels_gh <- function(item_labels) {
  lapply(item_labels, function(label) label$name) %>%
    unlist()
}

make_issue <- function(issue) {
  structure(
    list(
      number = issue$number,
      title = issue$title,
      body = issue$body %||% "",
      repo = get_repo_from_url(issue$html_url),
      api_url = issue$url,
      html_url = issue$html_url,
      labels = extract_labels_gh(issue$labels),
      author = issue$user$login,
      created_at = issue$created_at,
      type = if ("pull_request" %in% names(issue)) "pr" else "issue"
    ),
    class = "issue"
  )
}
