#' @export
draft_issue <- function(filename = "issue.Rmd", tempdir = TRUE, overwrite = TRUE) {
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
    package = "issuecreep",
    create_dir = FALSE,
    edit = FALSE
  )

  default_repo <- get_repo_remote()
  if (!is.null(default_repo)) {
    swap_repo_yaml(file_path, default_repo)
  }

  rstudioapi::navigateToFile(file_path)

}


swap_repo_yaml <- function(file_path, default_repo) {
  document_yaml <- rmarkdown::yaml_front_matter(file_path)
  yaml_length <- length(strsplit(yaml::as.yaml(document_yaml), "\n")[[1]]) + 2 # + 2 for "---"
  document_yaml$output$`issuecreep::github_issue`$repo <- default_repo
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
