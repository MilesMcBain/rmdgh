
register_reprex_knitr_engine <- function() {
  knitr::knit_engines$set(reprex = function(options) {

    if (length(options$code) == 0) {
      stop("No code supplied to {reprex} chunk")
    }

    reprex_code <- if(length(options$code) == 1) {
      c(options$code, "\n")
      # so that if user supplied 1 line of code it's not interpreted as file path
    } else {
      options$code
    }


    reprex_output <- 
      reprex::reprex(
        input = reprex_code,
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
