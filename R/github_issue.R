
#' @export
github_issue <- function(
  repo,
  number = NULL,
  draft = TRUE,
  close_with_comment = FALSE,
  fig_width = 7,
  fig_height = 5,
  dev = "png",
  df_print = "default",
  math_method = "default",
  wrap = "preserve" # auto | none | preserve
) {

  if (!is.null(number) && !is.numeric(number)) {
    stop("unrecognisable issue number: ", number)
  }
  assert_github_exists(repo = repo, issue = number)
  if (is.null(number) && close_with_comment) {
    stop("Can't create a closed issue ('close_with_comment: yes' for new issue.)")
  }

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
  # 1. Remove everything up to and including the footer line output by
  #   render_issue_footer()
  # 2. Call github_document preprocessor
  pre_processor <- function(metadata, input_file, ...) {
    input_lines <-
      xfun::read_utf8(input_file) %>%
      enc2utf8()

    footer_line <-
      grepl(
        render_issue_footer(),
        input_lines
      ) %>%
      which()

    if (length(footer_line) == 0) { 
      # There was no footer so assume it's a fresh issue
      # We just need to crop the yaml
      yaml_fences <-
        grepl(
          "^---",
          input_lines
        ) %>%
        which()
      
      if (length(yaml_fences) >= 2) {
        footer_line <- yaml_fences[2]
      } else{
        # Send everything
        footer_line <- 0
      }
    }

    output_lines <- if (footer_line > 0) {
      input_lines[-seq(footer_line)]
    } else {
      input_file
    }

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


    if (is.null(number)) {
      github_issue_submit(
        repo = repo,
        title = metadata$title,
        body = issue_body,
        draft = draft
      )
    } else {
      github_comment_submit(
        repo = repo,
        number = number,
        body = issue_body,
        draft
      )
    }
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


  github_document_format$pre_processor <- pre_processor
  github_document_format$post_processor <- post_processor
  github_document_format$on_exit <- on_exit

  github_document_format

}

github_issue_submit <- function(
  repo,
  title,
  body,
  draft = FALSE
) {

  query <-
    glue::glue("POST /repos/{repo}/issues")

  if (draft) {
    message("Did not submit ", query, "to GitHub, set 'draft: no' to submit.")
    return(invisible(NULL))
  }

  res <- gh::gh(
    query,
    title = title,
    body = body
  )

  message("GitHub issue created. See ", res$html_url)
}

github_comment_submit <- function(repo, number, body, draft = FALSE) {
  query <-
    glue::glue("POST /repos/{repo}/issues/{number}/comments")

  if (draft) {
    message("Did not submit ", query, "to GitHub, set 'draft: no' to submit.")
    return(invisible(NULL))
  }

  res <- gh::gh(
    query,
    body = body
  )

  message("GitHub issue comment submitted. See ", res$html_url)
}

github_issue_close <- function(repo, number) {

  res <- gh::gh(
    glue::glue("POST /repos/{repo}/issues/{number}"),
    state = "closed"
  )

  close_message <- glue::glue("{repo}#{number} was closed. ('close_with_comment: yes').")

  message(close_message)
}
