get_gh_email <- function() {
  gh_config <- gert::git_config()
  email <- gh_config[gh_config$name == "user.email", "value"]
  if (nrow(email) == 0) stop("Couldn't find user.email it git config")
  email$value
}

get_gh_user <- function() {
  user_email <- get_gh_email()
  res <- gh::gh(
    "/search/users",
    q = glue::glue("{user_email} in:email")
  )
  if (res$total_count == 0) stop("Could not find a GitHub user for ", user_email)
  if (res$total_count > 1) warning("Found more than one GitHub user for ", user_email)
  res$items[[1]]$login
}

is_open_qualifier <-  function(is_open) {
  if (is_open) "is:open" else ""
}

ISSUE_SEARCH_QUERY <-
  "{search_query} type:issue in:title,body repo:{resolved_package} sort:author-date-desc"

issues <- function(package, search_query = "", is_open = TRUE) {
  resolved_package <- resolve_package_repo(package)

  gh::gh(
    "/search/issues",
    q = glue::glue(
      ISSUE_SEARCH_QUERY,
      is_open_qualifier(is_open)
      )
  )
}

my_issues <- function(package, search_query = "", author = get_gh_user(), is_open = TRUE) {
  resolved_package <- resolve_package_repo(package)

  gh::gh(
    "/search/issues",
    q = glue::glue(
      paste(
        ISSUE_SEARCH_QUERY, 
        "author:{author}",
        is_open_qualifier(is_open)
        )
    )
  )
}

issues_with_me <- function(package, search_query = "", author = get_gh_user(), is_open = TRUE) {
  resolved_package <- resolve_package_repo(package)

  gh::gh(
    "/search/issues",
    q = glue::glue(
      paste(
        ISSUE_SEARCH_QUERY, 
        "involves:{author}",
        is_open_qualifier(is_open))
    )
  )

}
