render_issue_search_results <- function(issue_search_results) {

   issue_text <- lapply(issue_search_results, render_issue_one_line_description) 
   glue::glue("- {issue_text}") %>%
     glue::glue_collapse(sep = "\n") 
}

render_issue_one_line_description <- function(issue) {
  glue::glue("{issue$title}",
             " `{base_url_shortcode(issue$html_url)} {issue$repo}#{issue$number}`",
             " {paste0(issue$labels, collapse = \", \")}") %>%
             trimws()
}