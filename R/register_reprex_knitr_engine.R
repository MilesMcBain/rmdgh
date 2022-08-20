
register_reprex_knitr_engine <- function() {
  knitr::knit_engines$set(reprex = function(options) {

    reprex_output <- 
      reprex::reprex(
        input = options$code, 
        venue = "gh",
        html_preview = FALSE,
        tidyverse_quiet = TRUE,
        style = options$style %||% FALSE,
        std_out_err = options$std_out_err %||% FALSE,
        session_info = options$session_info %||% FALSE
        )

    glue::glue_collapse(reprex_output, sep = "\n")
  })
}
