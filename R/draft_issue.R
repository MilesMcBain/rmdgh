#' Create a GitHub issue from Rmarkdown
#'
#' An Rmarkdown document is created an opened that can add or comment on
#' issues/PRs when rendered with `rmarkdown::render()`, the 'knit' button in
#' RStudio, or the 'Knit Rmd' command in VSCode.
#'  
#' By default issues are created in a temporary dir, but this can be overidden
#' and they will be created in the current directory.
#' 
#' @param filename the Rmarkdown file name to contain your issue
#' @param tempdir use a temporary directory if TRUE, current working directory if FALSE
#' @param overwrite whether or not to overwrite an existing issue with the same filename (defaults to TRUE).
#' @export
draft_issue <- function(filename = "issue.Rmd", tempdir = getOption("draft_issue_in_tempdir", TRUE), overwrite = TRUE) {
  file_path <- if (tempdir) {
    file.path(get_pkg_user_dir(), filename)
  } else {
    file.path(getwd(), filename)
  }

  if (file.exists(file_path) && overwrite) {
    unlink(file_path)
  }

  rmarkdown::draft(
    file = file_path,
    template = "github_issue",
    package = "rmdgh",
    create_dir = FALSE,
    edit = FALSE
  )

  default_repo <- get_repo_remote()
  if (!is.null(default_repo)) {
    swap_repo_yaml(file_path, default_repo)
  }

  rstudioapi::navigateToFile(file_path)
  invisible(NULL)
}


swap_repo_yaml <- function(file_path, default_repo) {
  document_yaml <- rmarkdown::yaml_front_matter(file_path)
  yaml_length <- length(strsplit(yaml::as.yaml(document_yaml), "\n")[[1]]) + 2 # + 2 for "---"
  document_yaml$output$`rmdgh::github_issue`$repo <- default_repo
  document_lines <- xfun::read_utf8(file_path)
  new_document_lines <- c(
   paste0( 
     "---\n",
      yaml::as.yaml(document_yaml),
      "---"
      ),
    document_lines[-seq(yaml_length)]
  )
  unlink(file_path)
  xfun::write_utf8(new_document_lines, file_path)
}
