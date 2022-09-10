#' Save an issue to the current working directory
#' 
#' A helper for collecting issue documents locally to worked on at a later time.
#' 
#' `folder` will be `./issues` by default but is configurable in option
#' `rmdgh_issue_location`. If it does not exist, it is created and added to the
#' `.Rbuildignore`.
#' @param filename the name to give the issue. Defaulted from issue repo and number in yaml. 
#' @param folder where to store the issue. The default is "./issues".
#' @export
save_issue <- function(filename = NULL, folder = getOption("rmdgh_issue_location", "./issues")) {

  issue_context <- rstudioapi::getSourceEditorContext()
  assert_is_rmd(issue_context)

  issue_yaml <- rmarkdown::yaml_front_matter(issue_context$path)
  assert_github_issue(issue_yaml)

  if (is.null(filename)) {
    repo <- issue_yaml$output$`rmdgh::github_issue`$repo %||% "unknown_repo"
    number <- issue_yaml$output$`rmdgh::github_issue`$number %||% "draft"
    suffix <- ifelse(number == "draft", format(Sys.time(), "_%Y%m%d_%H%M%S"), "")
    filename <- glue::glue("{gsub('/', '_', repo)}_{number}{suffix}.Rmd")
  }
  
  if (!dir.exists(folder)) {
    usethis::use_directory(folder, ignore = TRUE)
  }
  output_path <- file.path(folder, filename)

  file.copy(
    issue_context$path,
    output_path,
    overwrite = FALSE
  ) 

  message(
    glue::glue("Saved {issue_context$path} as {output_path}.")
  )
}
