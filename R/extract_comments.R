#' @export
extract_comments <- function(result) UseMethod("extract_comments", result)

#' @export
extract_comments.default <- function(result) {
  stop(
    "Can't extract comments from object with classes: ",
    paste(class(result), collapse = ", ")
  )
}

#' @export
extract_comments.gh_response <- function(result) {

  lapply(result, function(comment) {
    list(
      author = comment$user$login,
      created_at = comment$created_at,
      body = comment$body
    )
  })

}

#' @export
extract_comments.issue <- function(issue) {
  list(
    author = issue$author,
    created_at = issue$created_at,
    body = issue$body
  )
}
