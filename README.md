
# issuecreep

SUPER EXPERIMENTAL PROBABLY DON'T USE YET BUT GO FOR IT IF YOU'RE BRAVE OR NICK TIERNEY.

A work-in-progress R <-> GitHub productivity tool powered by RMarkdown.

Works in VSCode or RStudio via `{rstudioapi}`.

## Installation

You can install the development version of issuecreep from [GitHub](https://github.com/) with:

``` r
remotes::install_github("MilesMcBain/issuecreep")
```

## Search Operations

- `my_issues()` issues by author (defaults to you)
- `issues_with_me()` issues referring to author (defaults to you)
- `repo_issues()` issues for a given repo or repos (defaults to current repo)

You can change who 'me' or 'my' is. It defaults to you but if you do:

```r
  my_issues(
    repos = "tidyverse/dplyr",
    author = "hadley"
  )
```
And you'll get Hadley's issues for `{dplyr}`.

`repos` accepts multiple repos.

## Navigation Operations

Probably bind these to keys.

- `issue_seach_results_forward()`
  - get the next page of issue search results
- `issue_search_results_backward()`
  - get the previous page of search results
- `issue_search_results_expand()`
  - search forward on the current line for the identifier of an issue and if found preview the issue body inline with search results.
- `jump_to_issue_thread()` 
  - Search as per 'expand', but go to the issue thread rendered as an Rmd document. You can submit updates to the issue, comments to the thread, or close the issue.
- `jump_to_issue_webpage()` 
  - Search as per `expand`, but go to the issue thread on GitHub

## GitHub issue thread RMarkdown output

We have an Rmarkdown output format called `github_issue` that can be used to submit issues, issue updates, and issue comments to GitHub on document render.

### Drafting issues

  - `draft_issue()` Will create a new RMarkdown GitHub issue, defaulting to the current repo.

Config you can use looks something like this:

```
---
title: Example
author: MilesMcBain
output:
  issuecreep::github_issue:
    repo: MilesMcBain/issuecreep
    number: 8
    action: comment
    draft: no
    close_with_comment: no
---
```

  - `action` is one of 'create', 'update', 'comment'
    - `number` is only valid with 'update' or 'comment'
    = `action: update` let's you update the issue title and body. Comments cannot be updated.

### Commenting on Issues

You can type in the necessary metadata to make a comment in the draft you're given above. But much nicer to navigate to the issue thread with `jump_to_issue_thread()` described above. Metadata is automatically set up for to submit a comment on render in this case.

### Making a reprex

`reprex::reprex()` doesn't really work well inside a code chunk. You may not even need it though, since rendering a `github_issue()` does a similar thing to `{reprex}` so long as you render it in a fresh session. 

You can use `error = TRUE` in the chunk options to display error output instead of stopping.

However, I decided to make a `{reprex}` `{knitr}` engine, since I think the specific output from the `{reprex}` package is expected in some communities, and could cause confusion if it is absent.

So with the new engine you can make a code chunk that uses `{reprex}` instead of
`{r}`. The output will be as if you had called `reprex::reprex()` on the code in
that chunk. Code in these chunks is self-contained, as per regular reprexes.



# FAQ

## Why? Why have you done this?

I find typing into little text input boxes on GitHub.com a bit of a buzz kill
when drafting issues. My text editor feels so much nicer to draft technical
communications with.

There's also a bit of jankyness that comes from the fact that most of the time
when drafting issues I'll want a reprex, which has to be coded up in R, rendered, and then pasted into the issue. But sometimes as the issue evolves the
reprex needs to also, and there's awkward iterative back and forth involving context switching
between applications.

This is exactly the kind of source-output syncronisation and context switching pain that `{knitr}` and `{rmarkdown}` take away. Why wouldn't we draft GitHub issues and comments that mash up code and prose in RMarkdown?!

## Why are you using Rmd instead of Quarto?

I've never used Quarto! I need to walk before I can run with all this document
generation stuff. Also VSCode support for coding in R in Rmd is better than
coding in R in Qmd.

And RMarkdown being displaced by Quarto means at some point in the future it's
going to be all old school and vintage hipster cool. I'm just getting ahead of
the curve.

## Will you port this to Quarto at some point?

Possibly. I like the idea of being able to author issues that use multiple
languages. However much of this package works via the `{rstudioapi}` which means
it's not likely to appeal to quarto users from other languages.

Probably a Quarto version should be implemented as VSCode extension? But then
the potential user base is much smaller at present. Let's see what Posit does
with extensions in the stand-alone Quarto editor that must surely be coming.