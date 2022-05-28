get_gh_email <- function() {
  gh_config <- gert::git_config()
  email <- gh_config[gh_config$name == "user.email", "value"]
  if (nrow(email) == 0) stop("Couldn't find user.email it git config")
  email$value
}

get_gh_user <- function() {
  user_email <- get_gh_email()
  res <- gh(
    "/search/users",
    q = glue::glue("{user_email} in:email")
  )
  if (res$total_count == 0) stop("Could not find a GitHub user for ", user_email)
  if (res$total_count > 1) warning("Found more than one GitHub user for ", user_email)
  res$items[[1]]$login
}

