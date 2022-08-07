globalVariables("RESULTS_CACHE")

RESULTS_CACHE <- new.env()

cache_result <- function(gh_result_object) {
  key <- uuid::UUIDgenerate()
  gh_result_object$cache_key <- key
  assign(key, gh_result_object, envir = RESULTS_CACHE)
  gh_result_object
}

get_cached_result <- function(key) {
  get(key, envir = RESULTS_CACHE)
}

clear_results_cache <- function() {
  rm(list = ls(RESULTS_CACHE), envir = RESULTS_CACHE)
}

