render_issue_search_results <- function(issue_search_results) {

   issue_text <- lapply(issue_search_results, render_issue_one_line_description) 
   glue::glue("- {issue_text}") %>%
     glue::glue_collapse(sep = "\n") 
}

render_issue_one_line_description <- function(issue) {
  glue::glue("{issue$title} ",
             render_issue_shortcode(issue),
             " {paste0(issue$labels, collapse = \", \")}") %>%
             trimws()
}

render_issue_shortcode <- function(issue) {
  glue::glue("`{service_from_base_url(issue$html_url)} {issue$repo}#{issue$number}`")
}