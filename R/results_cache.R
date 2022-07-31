globalVariables("RESULTS_CACHE")

RESULTS_CACHE <- new.env()

cache_result <- function(gh_result_object) {
  key <- uuid::UUIDgenerate()
  assign(key, gh_result_object, envir = RESULTS_CACHE)
}

get_cached_result <- function(key) {
  get(key, envir = RESULTS_CACHE)
}


