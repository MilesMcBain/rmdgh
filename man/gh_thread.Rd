% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gh_thread.R
\name{gh_thread}
\alias{gh_thread}
\title{Open an issue thread}
\usage{
gh_thread(thread = NULL)
}
\arguments{
\item{thread}{text representing a thread to open, or nothing to try the clipboard. See details}
}
\description{
Three types of arugments are supported for opening an issue thread:
\itemize{
\item no argument (NULL), an attempt is made to read a url for a thread from the clipboard
\item numeric argument, an attempt is made to open an issue with this number in
the repo corresponding to the current local repository
\item A shorthand syntax: \code{"milesmcbain/capsule#12"} or \code{"capsule#12"}
}
}
