resolve_package_repo <- function(package) {
  if (is_qualified_packagename(package)) {
    assert_github_repo_exists(package)
    package
  } else if (is_installed_locally(package)) {
    resolve_package_from_local_lib(package)
  } else {
    resolve_package_from_CRAN(package)
  }
}

assert_github_repo_exists <- function(repo) {
  tryCatch(
    gh(
      glue::glue("/repos/{repo}")
    ),
    error = function(e) {
      stop("could not find repository on GitHub: ", repo)
    }
  )
}

is_installed_locally <- function(package) {
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
  url <- "https://github.com/miles-mcbain/datapasta"
  regexpr(
    "github.com/[A-Za-z0-9-]+/[A-Za-z.0-9]+",
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

  if (package_data$RemoteType == "github") {
    repo <- glue::glue("{package_data$RemoteUsername}/{package_data$RemoteRepo}")
  }
  else if (is_github_url(package_data$BugReports)) {
    repo <- get_repo_from_url(package_data$BugReports)
  } else {
    stop(
      "Couldn't resolve Github URL for: ",
      package,
      ".",
      " Try using a qualified name e.g. <github_account>/<repo>"
    )
  }

  assert_github_repo_exists(repo)
  repo
}

resolve_package_from_CRAN <- function(package) {
  CRAN_tables <-
    rvest::read_html(
      glue::glue("https://cran.r-project.org/package={package}")
    ) %>%
    rvest::html_table()

  package_data <-
    CRAN_tables[[1]] %>%
    setNames(c("field", "value")) %>%
    tidyr::pivot_wider(names_from = field, values_from = value) %>%
    setNames(., gsub(":", "", colnames(.)))

  resolve_from_package_data(package_data)
}
