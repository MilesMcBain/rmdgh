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
#' @param path the path to create the rmd issue in. Defaults to
#'  `tools::R_user_dir("rmdgh")`. Issues created in the default directory will be
#'  automatically cleaned up. Default can be changed with option `rmd_gh_issue_draft_path`.
#' @param overwrite whether or not to overwrite an existing issue with the same filename (defaults to TRUE).
#' @export
draft_issue <- function(
  filename = "issue.Rmd",
  path = getOption("rmdgh_issue_draft_path", get_pkg_user_dir()),
  overwrite = TRUE
) {
  file_path <- file.path(path, filename)

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
