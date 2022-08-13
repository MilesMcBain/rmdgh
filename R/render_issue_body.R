render_issue_body <- function(issue) {

  glue::glue("-----\n",
             "*@{issue$author} wrote {time_diff(issue$created_at)}*:\n",
             "{issue$body}\n",
             "-----\n", 
             .trim = FALSE)

}
