parse_issue_content <- function(issue_content) {

  issue_lines <- strsplit(issue_content, "\r\n")[[1]]

  line_is_chunk <- vapply(
    issue_lines,
    get_chunk_line_tracker(),
    logical(1),
    USE.NAMES = FALSE
  )

  is_chunk_start <- line_is_chunk & !lag_indicator(line_is_chunk)
  is_chunk_end <- line_is_chunk & !lead_indicator(line_is_chunk)
  is_code <- line_is_chunk & !is_chunk_start & !is_chunk_end

  line_leads_with_hash <- 
    grepl("^\\s{0,3}#", issue_lines) 
  # 4 or more spaces makes the output quoted

  is_header <-
    line_leads_with_hash & !line_is_chunk

  data.frame(
    line_number = seq_along(issue_lines),
    line = issue_lines,
    is_chunk = line_is_chunk,
    is_chunk_start = is_chunk_start,
    is_code = is_code,
    is_chunk_end,
    is_header
  )
}

get_chunk_line_tracker <- function() {
  in_code <- FALSE

  function(line) {
    if (grepl("^```", line)) in_code <<- !in_code
    in_code
  }
}
