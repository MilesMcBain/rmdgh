get_gh_email <- function() {
  email <- tryCatch(
    {
      gh_config <- gert::git_config()
      gh_config[gh_config$name == "user.email", "value"]
    },
    error = function(x) {
      NULL
    }
  )

  if (is.null(email)) {
    gh_config_global <- gert::git_config_global()
    email <- gh_config_global[gh_config_global$name == "user.email", "value"]
  }

  if (nrow(email) == 0) {
    stop("Couldn't find user.email it git config")
  }
  email$value
}

#' Get the github user using local git email config.
#' @export
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


#' @export
get_repo_remote <- function() {
  tryCatch(
    {
      remote <- gh::gh_tree_remote()
      glue::glue("{remote$username}/{remote$repo}")
    },
    error = function(x) NULL
  )
}

is_open_qualifier <- function(is_open) {
  if (is.null(is_open)) {
    return(NULL)
  }
  if (is_open) "is:open" else "is:closed"
}

when_supplied_make_kvp_else_null <- function(value, key) {
  if (!is.null(value) && length(value) > 0) {
    paste0(key, ":", "\"", value, "\"", collapse = " ")
  } else {
    NULL
  }
}

make_query_arg_list <- function(
  repos = NULL,
  search_query = NULL,
  type = "issue",
  search_in = c("title", "body"),
  author = NULL,
  involves = NULL,
  is_open = TRUE,
  label = NULL,
  query_description = NULL,
  order = "desc",
  extra_params = NULL
) {
  as.list(environment())
}

make_search_result <- function(
  issues,
  result_obj,
  query,
  query_description,
  cache_key,
  current_page,
  max_page
) {
  call_args <- as.list(environment())
  structure(
    call_args,
    class = "issue_search_result"
  )
}

issue_query <- function(
  repos = NULL,
  search_query = NULL,
  type = "issue",
  search_in = c("title", "body"),
  author = NULL,
  involves = NULL,
  is_open = TRUE,
  label = NULL,
  query_description = NULL,
  order = "desc",
  extra_params = NULL
) {
  call_args <- as.list(environment())
  resolved_repos <- lapply(repos, resolve_repo)
  
  if (!is.null(repos)) {
    call_args$repos <- resolved_repos
  }

  repos_kvp <- when_supplied_make_kvp_else_null(resolved_repos, "repo")
  author_kvp <- when_supplied_make_kvp_else_null(author, "author")
  involves_kvp <- when_supplied_make_kvp_else_null(involves, "involves")
  type_kvp <- when_supplied_make_kvp_else_null(type, "type")
  in_kvp <- when_supplied_make_kvp_else_null(search_in, "in")
  label_kvp <- when_supplied_make_kvp_else_null(label, "label")
  is_open_kvp <- is_open_qualifier(is_open)

  issue_search_query <-
    paste(
      search_query,
      repos_kvp,
      author_kvp,
      involves_kvp,
      type_kvp,
      in_kvp,
      label_kvp,
      is_open_kvp,
      paste(extra_params, collapse = " ")
    )

  result <- gh::gh(
    "/search/issues",
    q = glue::glue(
      issue_search_query
    ),
    per_page = getOption("issue_search_results_per_page", 30),
    sort = "updated",
    order = order
  )

  make_search_result(
    issues = extract_issues(result),
    result_obj = result,
    query = do.call(make_query_arg_list, call_args),
    query_description = query_description,
    current_page = 1L,
    max_page = as.integer(ceiling(result$total_count / getOption("issue_search_results_per_page", 30)))
  ) %>%
    cache_result()
}

return_search_result <- function(result) {
  if (length(result$issues) > 0) {
    display_issue_search_results(result)
  } else {
    message("No issue search results.")
  }
}

#' @describeIn issues issues for the local repository 
#' @export
repo_issues <- function(
  repos = get_repo_remote(),
  query_description = glue::glue("repository issues for {paste(repos, collapse = \" \")}"),
  ...
) {
  if (is.null(repos)) {
    stop(
      "'repos' was not supplied",
      "and could not be defaulted from current working directory."
    )
  }

  issues(
    repos = repos,
    query_description = query_description,
    ...
  )
}


#' @describeIn issues PRs for the local repository
#' @export
repo_prs <- function(
  repos = get_repo_remote(),
  query_description = glue::glue("repository PRs for {paste(repos, collapse = \" \")}"),
  ...
) {
  repo_issues(
    repos = repos,
    query_description = query_description,
    type = "pr",
    ...
  )
}

#' @describeIn issues issues authored by you
#' @export
my_issues <- function(
  author = get_gh_user(),
  query_description = glue::glue("{paste(author, collapse = \" \")} issues"),
  ...
) {
  issues(
    author = author,
    query_description = query_description,
    ...
  )
}

#' @describeIn issues issues referring to you
#' @export
issues_with_me <- function(
  involves = get_gh_user(),
  query_description = glue::glue("issues with {paste0(involves)}"),
  ...
) {
  issues(
    involves = involves,
    query_description = query_description,
    ...
  )
}


#' @describeIn issues PRs by you
#' @export
my_prs <- function(
  author = get_gh_user(),
  type = "pr",
  query_description = glue::glue("PRs by {paste0(author)}"),
  ...
) {
  issues(
    author = author,
    type = type,
    query_description = query_description,
    ...
  )
}

#' @describeIn issues PRs referring to you
#' @export
prs_with_me <- function(
  involves = get_gh_user(),
  type = "pr",
  query_description = glue::glue("PRs with {paste0(involves)}"),
  ...
) {
  issues(
    involves = involves,
    type = type,
    query_description = query_description,
    ...
  )
}

#' @describeIn issues PRs in repositories you own
#' @export
prs_for_me <- function(
  user = get_gh_user(),
  type = "pr",
  query_description = glue::glue("PRs for {paste0(user)}"),
  extra_params = glue::glue("user:{user}"),
  ...
) {
  issues(
    type = type,
    query_description = query_description,
    extra_params = extra_params,
    ...
  )
} 

#' @describeIn issues issues in repositories you own
#' @export
issues_for_me <- function (
  user = get_gh_user(),
  type = "issue",
  query_description = glue::glue("Issues for {paste0(user)}"),
  extra_params = glue::glue("user:{user}"),
  ...
) {
  issues(
    type = type,
    query_description = query_description,
    extra_params = extra_params,
    ...
  )
} 

#' @describeIn issues all issues and PRs in repositories you own.
#' @export
gh_for_me <- function (
  user = get_gh_user(),
  type = NULL,
  query_description = glue::glue("Issues and PRs for {paste0(user)}"),
  extra_params = glue::glue("user:{user}"),
  ...
) {
  issues(
    type = type,
    query_description = query_description,
    extra_params = extra_params,
    ...
  )
} 

#' Search Issues and PRs and present results in Rmarkdown
#' 
#' `issues()` and its user-friendly wrappers are designed to allow you quick
#' access to lists of issues and PRs that you can use as a jumping off point for
#' exploring Rmarkdown issue threads. 
#' 
#' Results are paged according to `getOption('issue_search_results_per_page')`.
#' 
#' Navigate between pages with [issue_search_results_forward()] and [issue_search_results_backward()].
#' 
#' Previw an issue thread inline with search results with [issue_search_results_expand()].
#'
#' Use [jump_to_issue_thread()] to jump to an Rmarkdown thread based on the
#' cursor position or [jump_to_issue_webpage()] to jump to the web.
#' 
#' @param repos a character vector issue repositories to get issues from. "owner/repo"
#'   and "repo" are allowed, with the repo resolved against
#'   installed R packages and CRAN in the latter case.
#' @param search_query a character string to search for in issue titles and bodies.
#' @param type `"issue"` or `"pr"` or NULL for both kinds.
#' @param search_in where to apply `search query`. Any combination of `"title"` or `"body"`. The default is `c("title", "body")` for both.
#' @param author a character vector of issue authors to return issues by.
#' @param involves a character vector of users to find in issue/PR threads, filtering returned results.
#' @param is_open TRUE for open issues, FALSE for closed, NULL for both.
#' @param label a character vector of issue labels to filter reutrned results.
#' @param query_description describe this query (appears in Rmarkdown results). Useful for building higher level functionality.
#' @param order `"asc"` or `"desc"` - the ordering of search results by last update date.
#' @param extra_params a character vector of extra query parameters that are passed verbatim to the github API. This take the form `"key:value"` e.g. `org:reditorsupport`.
#' @export
issues <- function(
  repos = NULL,
  search_query = NULL,
  type = "issue",
  search_in = c("title", "body"),
  author = NULL,
  involves = NULL,
  is_open = TRUE,
  label = NULL,
  query_description = NULL,
  order = "desc",
  extra_params = NULL
) {
  issue_args <- as.list(environment())
  do.call(issue_query, issue_args) %>%
    return_search_result()
}
