create_temp_document <- function(
  document_title,
  document_front_matter,
  document_body
) {
  document_path <- file.path(get_pkg_user_dir(), md_document(document_title))
  document_content <-
    paste0(
      document_front_matter,
      "\n",
      document_body
    )
  writeLines(
    document_content,
    document_path
  )
  document_path
}

clean_up_temp_doucments <- function() {

}

get_pkg_user_dir <- function() {
  pkg_user_dir <- tools::R_user_dir("rmdgh")
  if (!dir.exists(pkg_user_dir)) {
    dir.create(pkg_user_dir, recursive = TRUE)
  }
  pkg_user_dir
}

md_document <- function(document_tile) {
  paste0(document_tile, ".Rmd")
  # In future we'll need to check if user is rolling Qmd or Rmd.
}

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "dir.exists")
  backports::import(pkgname, "R_user_dir", force = TRUE)
  
  # clean up
  pkg_user_dir <- get_pkg_user_dir()
  file_info_df <- fs::file_info(list.files(pkg_user_dir, full.names = TRUE))
  # Remove all files from previous days
  old_files <- as.numeric(Sys.Date() - as.Date(file_info_df$birth_time, tz = Sys.timezone())) > 0
  unlink(file_info_df[old_files, ]$path)

  # register reprex engine
  register_reprex_knitr_engine()
}
