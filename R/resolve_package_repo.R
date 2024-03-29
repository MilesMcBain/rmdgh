resolve_repo <- function(repo, check_github_exists = TRUE) {
  if (is_qualified_repo_name(repo)) {
    # e.g. tidyverse/dplyr
    if (check_github_exists) assert_github_exists(repo = repo)
    repo
    # if it's not a qualified name, try to resolve it as an R package name
  } else if (is_r_package_installed_locally(repo)) {
    resolve_package_from_local_lib(repo)
  } else {
    assert_CRAN_page_exists(repo)
    resolve_package_from_CRAN(repo)
  }
}

is_qualified_repo_name <- function(repo) {
  grepl("^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$", repo)
}


is_r_package_installed_locally <- function(package) {
  tryCatch(
    length(find.package(package)) > 0,
    error = function(e) FALSE
  )
}

resolve_package_from_local_lib <- function(package) {
  package_data <-
    find.package(package) %>%
    file.path("DESCRIPTION") %>%
    read.dcf() %>%
    as.data.frame()

  resolve_from_package_data(package_data)
}

is_github_url <- function(url) {
  if (is.null(url) || is.na(url)) {
    return(FALSE)
  }
  grepl("github.com", url)
}


get_repo_from_url <- function(url) {
  regexpr(
    "github.com/[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+",
    url,
    ignore.case = TRUE
  ) %>%
    regmatches(
      url,
      .
    ) %>%
    gsub(
      "github.com/",
      "",
      .,
      ignore.case = TRUE
    )
}

resolve_from_package_data <- function(package_data) {

  if (!is.null(package_data$RemoteType) && package_data$RemoteType == "github") {
    repo <- glue::glue("{package_data$RemoteUsername}/{package_data$RemoteRepo}")
  }
  else if (is_github_url(package_data$BugReports)) {
    repo <- get_repo_from_url(package_data$BugReports)
  } else if (is_github_url(package_data$RemoteUrl)) {
    repo <- get_repo_from_url(package_data$RemoteUrl)
  } else {
    stop(
      "Couldn't resolve Github URL for: ",
      package_data$Package,
      ".",
      " Try using a qualified name e.g. <github_account>/<repo>"
    )
  }

  assert_github_exists(repo = repo)
  repo
}

resolve_package_from_CRAN <- function(package) {
  CRAN_tables <-
    rvest::read_html(
      cran_url(package)
    ) %>%
    rvest::html_table()

  package_data <-
    CRAN_tables[[1]] %>%
    stats::setNames(c("field", "value")) %>%
    tidyr::pivot_wider(names_from = field, values_from = value) %>%
    stats::setNames(., gsub(":", "", colnames(.)))

  resolve_from_package_data(package_data)
}

cran_url <- function(package) {
  glue::glue("https://cran.r-project.org/package={package}")
}

package_name <- function(resolved_package) {
  gsub("^[A-Za-z0-9-]+/", "", resolved_package)
}
