get_issue_thread <- function(matching_issue) {

  result <- gh::gh(
    glue::glue("/repos/{matching_issue$repo}/issues/{matching_issue$number}/comments")
  )

  initial_comment <- extract_comments(
    matching_issue
  )
  comments <- extract_comments(
    result
  )

  structure(
    list(
      title = matching_issue$title,
      author = matching_issue$author,
      repo = matching_issue$repo,
      number = matching_issue$number,
      thread =
        c(
          list(initial_comment),
          comments
        )
    ),
    class = "issue_thread"
  )
}
