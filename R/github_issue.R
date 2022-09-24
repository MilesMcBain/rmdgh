#' A github issue Rmarkdown format
#' 
#' A wraper for [rmarkdown::github_document()] that can peform actions on GitHub
#' when rendering relating to issues and PRs including:
#'   - create an issue
#'   - update an issue title, body, and labels
#'   - comment on an issue
#'   - close an issue with a comment
#' @param repo the repository create the issue on e.g. "milesmcbain/capsule", or
#'  "capsule". Name will be resolved against locally insalled packages and CRAN in
#'  the later case.
#' @param number the issue number in the repository if performing a comment or update action
#' @param labels the labels to set for the issue if performing create or update action
#' @param action the type of action to perform: "create", "update", "comment". Only "comment" is valid for PRs.
#' @param draft if TRUE the action is not performed after rendering the document to
#'  GitHub markdown - set to TRUE initially to give you a chance to preview
#'  markdown output.
#' @param close_with_comment if TRUE close the issue thread with comment action (assuming you have appropriate repo permissions).
#' @param fig_width passed to image dimension passed to [rmarkdown::github_document()]
#' @param fig_height image dimension passed to image dimension passed to [rmarkdown::github_document()]
#' @param dev graphics device argument passed to [rmarkdown::github_document()]
#' @param df_print data.frame print method passed to [rmarkdown::github_document()]
#' @param math_method latex style math expression rendering method passed [rmarkdown::github_document()]
#' @param wrap argument passed to pandoc. Controls use of line breaks. One of
#' "auto", "preserve" or "none". "preserve" will mostly not interfere with your
#' text formatting.
#' @export
github_issue <- function(
  repo = NULL,
  number = NULL,
  labels = NULL,
  action = "create", # create | update | comment
  draft = TRUE,
  close_with_comment = FALSE,
  fig_width = 7,
  fig_height = 5,
  dev = "png",
  df_print = "default",
  math_method = "default",
  wrap = "preserve" # auto | none | preserve
) {
  if(is.null(repo)) {
    stop("'repo' option must be supplied")
  }
  if (!is.null(number) && !is.numeric(number)) {
    stop("unrecognisable issue number: ", number)
  }
  repo <- resolve_repo(repo, check_github_exists = FALSE)
  assert_github_exists(repo = repo, issue = number)
  if (is.null(number) && close_with_comment) {
    stop("Can't create a closed issue ('close_with_comment: yes' for new issue.)")
  }
  if (!(action %in% c("create", "update", "comment"))) {
    stop("action must be one of create, update or comment")
  }
  if (action == "create" && !is.null(number)) {
    stop("Supplied an issue number for 'create' action (default). Only applicable for 'update' or 'comment'")
  }
  upload_fun <- NULL
  if (draft) {
    upload_fun <- function(x) x
  }
  upload_fun <- upload_fun %||% knitr::opts_chunk$get("upload.fun") %||% knitr::imgur_upload
  

  github_document_format <- rmarkdown::github_document(
    fig_width = fig_width,
    fig_height = fig_height,
    dev = dev,
    df_print = df_print,
    math_method = math_method,
    html_preview = draft,
    pandoc_args = c("--wrap", wrap)
  )

  github_document_format_pre_processor <-
    github_document_format$pre_processor %||%
    function(...) character(0L)

  github_document_format_post_processor <-
    github_document_format$post_processor %||%
    function(metadata, input_file, output_file, ...) output_file

  # Preprocessor:
  # 1.1 If action is 'comment' Remove everything up to and including the footer line output by
  #   render_issue_footer()
  # 1.2 If action is 'create' Remove the yaml only
  # 1.3 If action is 'update' Remove everything below and includding `render_issue_body_footer()`.
  #  - Remove the yaml
  #  - Remove the attribution / time stamp line
  # 2. Call github_document preprocessor
  pre_processor <- function(metadata, input_file, ...) {
    input_lines <-
      xfun::read_utf8(input_file) %>%
      enc2utf8()

    output_lines <-
      switch(
        action,
        create = get_output_lines_for_create(input_lines),
        comment = get_output_lines_for_comment(input_lines),
        update = get_output_lines_for_update(input_lines, metadata$author),
        stop("Unknown issue action: ", action)
      )

    xfun::write_utf8(
      output_lines,
      input_file
    )
    github_document_format_pre_processor(
      metadata,
      input_file,
      ...
    )
  }

  # Postprocessor:
  # 1. If draft is FALSE, submit the issue to GitHub
  #   - If number is not null, it's for an existing thread as a comment
  #   - If number is null is for a new issue thread
  # 2. Call github_document Postprocessor
  post_processor <- function(metadata, input_file, output_file, ...) {

    issue_body <-
      xfun::read_utf8(output_file) %>%
      glue::glue_collapse(sep = "\n")

    switch(
      action,
      create = github_issue_submit(
        repo = repo,
        title = metadata$title,
        body = issue_body,
        labels = labels,
        draft = draft
      ),
      comment = github_comment_submit(
        repo = repo,
        number = number,
        body = issue_body,
        draft = draft
      ),
      update = github_update_submit(
        repo = repo,
        number = number,
        title = metadata$title,
        body = issue_body,
        labels = labels,
        draft = draft
      ),
      stop("Unknown issue action: ", action)
    )

    withr::with_envvar(
      c(RMARKDOWN_PREVIEW_DIR = get_pkg_user_dir()),
      github_document_format_post_processor(
        metadata,
        input_file,
        output_file,
        ...
      )
    )
  }

  # on exit:
  on_exit <- function() {
    if (close_with_comment && !draft) {
      assert_github_exists(repo, number)
      github_issue_close(
        repo = repo,
        number = number
      )
    }
  }

  github_document_format$knitr$opts_knit <-  
    modifyList(github_document_format$knitr$opts_knit %||% list(), list(upload.fun = upload_fun))
  github_document_format$pre_processor <- pre_processor
  github_document_format$post_processor <- post_processor
  github_document_format$on_exit <- on_exit
  github_document_format

}

github_issue_submit <- function(
  repo,
  title,
  body,
  labels,
  draft = FALSE
) {

  query <-
    glue::glue("POST /repos/{repo}/issues")

  if (draft) {
    just_a_draft(query)
    return(invisible(NULL))
  }

  res <- gh::gh(
    query,
    encode_issue_json(
      title = title,
      body = body,
      labels = labels
    ),
    .send_headers = c(
      Accept = "application/vnd.github.switcheroo-preview+json",
      "Content-Type" = "application/json"
    ) # because using manual JSON encoding so labels is always an array.
  )

  message("\nGitHub issue created. See ", res$html_url)
}

github_comment_submit <- function(repo, number, body, draft = FALSE) {
  assertthat::assert_that(
    nzchar(body),
    msg = "Cannot submit an empty issue comment."
  )
  query <-
    glue::glue("POST /repos/{repo}/issues/{number}/comments")

  if (draft) {
    just_a_draft(query)
    return(invisible(NULL))
  }

  res <- gh::gh(
    query,
    body = body
  )

  message("\nGitHub issue comment submitted. See ", res$html_url)
}

github_update_submit <- function(repo, number, title, body, labels, draft = FALSE) {

  query <- glue::glue("PATCH /repos/{repo}/issues/{number}")

  if (draft) {
    just_a_draft(query)
    return(invisible(NULL))
  }


  res <- gh::gh(
    query,
    encode_issue_json(
      title = title,
      body = body,
      labels = labels
    ),
    .send_headers = c(
      Accept = "application/vnd.github.switcheroo-preview+json",
      "Content-Type" = "application/json"
    ) # because using manual JSON encoding so labels is always an array.
  )

  message("\nGitHub issue update submitted. See ", res$html_url)
}

github_issue_close <- function(repo, number) {

  res <- gh::gh(
    glue::glue("POST /repos/{repo}/issues/{number}"),
    state = "closed"
  )

  close_message <- glue::glue("{repo}#{number} was closed. ('close_with_comment: yes').")

  message(close_message)
}

get_output_lines_for_create <- function(input_lines) {
  yaml_fences <-
    grepl(
      "^---",
      input_lines
    ) %>%
    which()

  output_lines <-
    if (length(yaml_fences) >= 2) {
      footer_line <- yaml_fences[2]
      input_lines[-seq(footer_line)]
    } else {
      # Send everything
      input_lines
    }
}

get_output_lines_for_comment <- function(input_lines) {
  footer_line <-
    grepl(render_issue_footer(), input_lines) %>%
    which()

  input_lines[-seq(footer_line)]
}

get_output_lines_for_update <- function(input_lines, author) {
  delete_below <-
    grepl(render_issue_body_footer(), input_lines) %>%
    which()

  body_lines <-
    input_lines[1:(delete_below - 1)]

  footer_line <-
    grepl(
      glue::glue("@{author}"),
      input_lines
    ) %>%
    match(TRUE, .)

  if (is.na(footer_line)) {
    stop("Could not find author attribution line in issue for action 'update'.")
  }

  body_lines[-seq(footer_line)] %>%
    trim_leading_and_trailing_blanks()

}

just_a_draft <- function(query) message("\nDid not submit ", query, " to GitHub, set 'draft: no' to submit.\n")

trim_leading_and_trailing_blanks <- function(text_lines) {
  line_mask <- rep(FALSE, length(text_lines))
  line_mask[1] <- TRUE
  line_mask[length(text_lines)] <- TRUE
  lines_to_drop <- (text_lines == "") & line_mask

  text_lines[!lines_to_drop]
}

encode_issue_json <- function(title, body, labels) {
  jsonlite::toJSON(
    list(
      title = jsonlite::unbox(title),
      body = jsonlite::unbox(body),
      labels = labels %||% character(0) # this always needs to be an array (boxed)
    )
  ) %>%
    charToRaw()
}
