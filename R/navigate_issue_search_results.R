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

issue_search_results_expand <- function() {
  document_context <- rstudioapi::getActiveDocumentContext()

  shortcode <- match_shortcode(document_context)
  if (length(shortcode) == 0) message("nothing to expand.")

  issue_url <- make_issue_url(shortcode)
  
  issue_search_results <- get_search_results_from_yaml()

  search_result_urls <- vapply(
    issue_search_results$issues,
    function(issue) issue$html_url,
    character(1)
  )

  matching_issue <- issue_search_results$issues[search_result_urls == issue_url]

  browser()

  matching_issue$body
  
}

issue_search_results_collapse <- function() {}

jump_to_issue_webpage <- function() {
  document_context <- rstudioapi::getActiveDocumentContext()
  shortcode <- match_shortcode(document_context)
  issue_url <- make_issue_url(shortcode)
  browseURL(issue_url)
}

match_shortcode <- function(document_context) {
  content_line <- 
    atcursor::get_cursor_line(document_context)

  cursor_col <-
    atcursor::get_cursor_col(rstudioapi::primary_selection(document_context))
  
  match <- forward_match_shortcode(content_line, cursor_col)
  
  match
}

forward_match_shortcode <- function(content, position) {
  match <- gregexec("`[a-z]{2}\\s[A-Za-z_./-]+#[0-9]+`", content)[[1]]

  if (all(match < 0)) return(NULL)

  match_starts <- match
  match_ends <- match_starts + attr(match, "match.length") - 1 # need to include first character at match start
  shortcode_start <- match[!(match_ends < position)]
  shortcode_end <- match_ends[!(match_ends < position)]
  
  if (length(shortcode_start) == 0) return(NULL) # no matches ahead of cursor

  substr(content, shortcode_start[[1]], shortcode_end[[1]])
}

make_issue_url <- function(shortcode) {
  service <- regmatches(
    shortcode,
    regexpr("(?<=`)[a-z]{2}", shortcode, perl = TRUE)
  )

  repo <- regmatches(
    shortcode,
    regexpr("(?<=\\s)[A-Za-z_./-]+", shortcode, perl = TRUE)
  )

  issue_number <- regmatches(
    shortcode,
    regexpr("(?<=#)[0-9]+", shortcode, perl = TRUE)
  )

  switch(
    service,
    gh = glue::glue("https://github.com/{repo}/issues/{issue_number}"),
    stop("unknown shortcode service: ", service)
  )

}