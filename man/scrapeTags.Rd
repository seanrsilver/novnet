% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrapeTags.R
\name{scrapeTags}
\alias{scrapeTags}
\title{scrapeTags()
   a) reads a file with NovNet-style tags
   b) scrapes the alternate names
   c) writing to a Char.csv file
   d) It will also repopulate the -scrapeTags.csv with metadata from
      an associated Char.csv file, if one exists.}
\usage{
scrapeTags(filename, local = FALSE, include.all = TRUE)
}
\arguments{
\item{filename}{Character string of text name, with associated -tagNames file.}

\item{local}{Logical vector.  If FALSE (default), looks in Google Drive for files.  If TRUE, looks for filename in a folder with path data/filename/.}

\item{include.all}{Logical vector or numerical range. If TRUE (default), processes whole text.  If range (i.e. 1:18), processes only those chapters.  If FALSE, will offer a prompt to determine a range.}
}
\description{
description
}
\keyword{NovNet}
\keyword{PhaseII}
\keyword{Utilities}
