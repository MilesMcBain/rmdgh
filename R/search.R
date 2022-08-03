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

is_open_qualifier <- function(is_open) {
  if (is_open) "is:open" else NULL
}

when_supplied_make_kvp_else_null <- function(value, key) {
  if (!is.null(value) && length(value) > 0) {
    paste0(key, ":", value, collapse = " ")
  } else {
    NULL
  }
}

make_query_arg_list <- function(
  packages = NULL,
  search_query = NULL,
  author = NULL,
  involves = NULL,
  is_open = TRUE
) {
  as.list(environment())
}

make_search_result <- function(
  issues,
  result_obj,
  query,
  query_description
) {
  call_args <- as.list(environment())
  structure(
    call_args,
    class = "issue_search_result"
  )
}

issue_query <- function(
  packages = NULL,
  search_query = NULL,
  author = NULL,
  involves = NULL,
  is_open = TRUE
) {
  resolved_packages <- lapply(packages, resolve_package_repo)

  repos_kvp <- when_supplied_make_kvp_else_null(resolved_packages, "repo")
  author_kvp <- when_supplied_make_kvp_else_null(author, "author")
  involves_kvp <- when_supplied_make_kvp_else_null(involves, "involves")
  is_open_kvp <- is_open_qualifier(is_open)

  issue_search_query <-
    paste(
      search_query,
      "type:issue",
      "in:title,body",
      repos_kvp,
      author_kvp,
      involves_kvp,
      "sort:author-date-desc",
      is_open_kvp
    )

  result <- gh::gh(
    "/search/issues",
    q = glue::glue(
      issue_search_query
    )
  )

  result
}

pkg_issues <- function(package, search_query = "", is_open = TRUE) {
  call_args <- as.list(environment())

  result <- issue_query(
    package = package,
    search_query = search_query,
    is_open = is_open
  )

  make_search_result(
    issues = extract_issues(result),
    result_obj = result,
    query = do.call(make_query_arg_list, call_args),
    query_description = "package issues"
  ) %>%
    return_search_result()
}

my_issues <- function(package = NULL, search_query = "", author = get_gh_user(), is_open = TRUE) {
  call_args <- as.list(environment())

  result <- issue_query(
    package = package,
    search_query = search_query,
    author = author,
    is_open = is_open
  )

  make_search_result(
    issues = extract_issues(result),
    result_obj = result,
    query = do.call(make_query_arg_list, call_args),
    query_description = glue::glue("{paste0(author, collapse = \" \")} issues")
  ) %>%
    return_search_result()
}

issues_with_me <- function(package = NULL, search_query = "", involves = get_gh_user(), is_open = TRUE) {
  call_args <- as.list(environment())

  result <- issue_query(
    package = package,
    search_query = search_query,
    involves = involves,
    is_open = is_open
  )

  make_search_result(
    issues = extract_issues(result),
    result_obj = result,
    query = do.call(make_query_arg_list, call_args),
    query_description = glue::glue("issues with {paste0(involves)}")
  ) %>%
    return_search_result()
}

return_search_result <- function(result) {
  if (length(result$issues) > 0) {
    display_issue_search_results(result)
  } else {
    message("No issue search results.")
  }
}
