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
    order = "desc"
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

return_search_result <- function(result) {
  if (length(result$issues) > 0) {
    display_issue_search_results(result)
  } else {
    message("No issue search results.")
  }
}

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
  extra_params = NULL
) {
  issue_args <- as.list(environment())
  do.call(issue_query, issue_args) %>%
    return_search_result()
}
