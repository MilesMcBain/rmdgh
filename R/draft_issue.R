#' @export
draft_issue <- function(filename = "issue.Rmd", tempdir = TRUE, overwrite = TRUE) {
  file_path <- if (tempdir) {
    file.path(get_pkg_user_dir(), filename)
  }  else {
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

  rstudioapi::navigateToFile(file_path)

}