
#' @export
issue_search_results_forward <- function() {

  issue_search_results <- get_search_results_from_yaml()

  if (issue_search_results$current_page == issue_search_results$max_page) {
    message("No further search result pages.")
    return()
  }

  new_result_object <- gh::gh_next(issue_search_results$result_obj)

  make_search_result(
    issues = extract_issues(new_result_object),
    result_obj = new_result_object,
    query = issue_search_results$query,
    query_description = issue_search_results$query_description,
    current_page = issue_search_results$current_page + 1L,
    max_page = issue_search_results$max_page,
    cache_key = issue_search_results$cache_key
  ) %>%
    update_cached_result(key = issue_search_results$cache_key) %>%
    display_issue_search_results()

}

#' @export
issue_search_results_backward <- function() {

  issue_search_results <- get_search_results_from_yaml()

  if (issue_search_results$current_page == 1) {
    message("No prior search results pages.")
    return()
  }

  new_result_object <- gh::gh_prev(issue_search_results$result_obj)

  make_search_result(
    issues = extract_issues(new_result_object),
    result_obj = new_result_object,
    query = issue_search_results$query,
    query_description = issue_search_results$query_description,
    current_page = issue_search_results$current_page - 1L,
    max_page = issue_search_results$max_page,
    cache_key = issue_search_results$cache_key
  ) %>%
    update_cached_result(key = issue_search_results$cache_key) %>%
    display_issue_search_results()
}

#' @export
issue_search_results_expand <- function() {
  matching_issue <- get_issue_from_cursor_context()
  render_issue_body(matching_issue) %>%
    insert_text_below_cursor_line()
}


#' @export
jump_to_issue_thread <- function() {
  matching_issue <- get_issue_from_cursor_context()
  issue_thread <- get_issue_thread(matching_issue)

  display_issue_thread(issue_thread)
}

#' @export
refresh_issue_thread <- function() {
  document_context <- rstudioapi::getActiveDocumentContext()
  assert_is_rmd(document_context)
  issue_yaml <- rmarkdown::yaml_front_matter(document_context$path)
  assert_github_issue(issue_yaml)

  github_issue <- issue_yaml$output[[1]]
  issue <-
   gh::gh(
      api_issue_query(repo = github_issue$repo, number = github_issue$number)
    )
  issue_thread <-
    get_issue_thread(
      extract_issues(issue)
    )

  replace_issue_thread(issue_thread, document_context)

}

#' @export
jump_to_issue_webpage <- function() {
  issue <- get_issue_from_cursor_context()
  utils::browseURL(issue$html_url)
}

match_issue_reference <- function(document_context) {
  content_line <-
    atcursor::get_cursor_line(document_context)

  cursor_col <-
    atcursor::get_cursor_col(rstudioapi::primary_selection(document_context))

  match <-
    forward_match_shortcode(content_line, cursor_col) %||%
    forward_match_hashref(content_line, cursor_col)

  match
}

forward_match_regex <- function(content, position, regex) {
  match <- gregexec(regex, content)[[1]]

  if (all(match < 0)) {
    return(NULL)
  }

  match_starts <- match
  match_ends <- match_starts + attr(match, "match.length") - 1 # need to include first character at match start
  shortcode_start <- match[!(match_ends < position)]
  shortcode_end <- match_ends[!(match_ends < position)]

  if (length(shortcode_start) == 0) {
    return(NULL)
  } # no matches ahead of cursor

  substr(content, shortcode_start[[1]], shortcode_end[[1]])
}

forward_match_shortcode <- function(content, position) {
  match <- forward_match_regex(content, position, "`[a-z]{2}\\s[A-Za-z0-9_./-]+#[0-9]+`")

  if (is.null(match)) {
    return(match)
  }

  structure(
    match,
    class = "shortcode"
  )
}

forward_match_hashref <- function(content, position) {
  match <- forward_match_regex(content, position, "#[0-9]+\\b")

  if (is.null(match)) {
    return(match)
  }

  structure(
    match,
    class = "hashref"
  )

}

get_issue_from_cursor_context <- function() {
  document_context <- rstudioapi::getActiveDocumentContext()

  reference <- match_issue_reference(document_context)

  if (length(reference) == 0) {
    message("the cursor is not on or ahead of an issue code.")
    return(invisible(NULL))
  }

  issue_info <- make_issue_info(reference, document_context)

  matching_issue <- get_maybe_cached_issue(document_context, issue_info)

}

get_search_results_from_yaml <- function() {
  active_doc <- rstudioapi::getSourceEditorContext()

  doc_yaml <- rmarkdown::yaml_front_matter(active_doc$path)
  if (length(doc_yaml) == 0) stop("Found no yaml front matter in the current document.")

  search_cache_key <- doc_yaml$cache_key
  if (is.null(search_cache_key)) stop("Found no cache_key in the document yaml front matter.")

  issue_search_results <- get_cached_result(search_cache_key)
  if (is.null(issue_search_results)) stop("cache_key matched no search results - they're probably expired. Search again!")

  issue_search_results
}