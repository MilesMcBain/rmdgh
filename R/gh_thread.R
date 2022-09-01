#' Open an issue thread
#'
#' Three types of arugments are supported for opening an issue thread:
#'   - no argument (NULL), an attempt is made to read a url for a thread from the clipboard
#'   - numeric argument, an attempt is made to open an issue with this number in
#'     the repo corresponding to the current local repository
#'   - A shorthand syntax: `"milesmcbain/capsule#12"` or `"capsule#12"`
#' @param thread text representing a thread to open, or nothing to try the clipboard. See details
#' @export
gh_thread <- function(thread = NULL) {
  if (is.null(thread)) {
    message("attempting to read an issue thread url from clipboard...")
    input <- clipr::read_clip()
    if (is_gh_url(input)) {
      issue_info <- extract_issue_info_from_gh_url(input)
    }
  } else if (is.numeric(thread)) {
    repo <- get_repo_remote()
    issue_info <-
      structure(
        list(
          repo = repo,
          number = thread,
          api_url = gh_api_url(repo, thread)
        ),
        class = "issue_info"
      )
  } else if (is_issue_shorthand(thread)) {
    issue_info <- extract_issue_info_from_shorthand(thread)
  } else {
    stop("Could not identify issue from context.")
  }
  issue <- get_issue_from_issue_info(issue_info)
  issue_thread <- get_issue_thread(issue)

  display_issue_thread(issue_thread)
}
