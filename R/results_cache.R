globalVariables("RESULTS_CACHE")

RESULTS_CACHE <- new.env()

cache_result <- function(gh_result_object) {
  key <- uuid::UUIDgenerate()
  gh_result_object$cache_key <- key
  assign(key, gh_result_object, envir = RESULTS_CACHE)
  gh_result_object
}

update_cached_result <- function(gh_result_object, key) {
  assign(key, gh_result_object, envir = RESULTS_CACHE)
  gh_result_object
}

get_cached_result <- function(key) {
  tryCatch(
    get(key, envir = RESULTS_CACHE),
    error = function(e) NULL
  )
}


clear_results_cache <- function() {
  rm(list = ls(RESULTS_CACHE), envir = RESULTS_CACHE)
}

