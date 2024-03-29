% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoReplaceList.R
\name{autoReplaceList}
\alias{autoReplaceList}
\title{autoReplaceList()}
\usage{
autoReplaceList(filename = filename.v, patterns = patterns.l,
  local = TRUE)
}
\arguments{
\item{filename}{File name as character string, i.e. "Crusoe".}

\item{patterns}{List of length-2 vectors, format c("pattern", "replace"). Expects correctly formatted regular expressions.}

\item{local}{Default = TRUE. If FALSE, searches for file in google drive. If TRUE, seeks file in folder filename/.}
}
\description{
This function is a wrapper for autoReplace.  It searches a NovNet .txt file
for a correctly formatted regular expression and substitutes a replacement.
}
\details{
It is useful for deleting page numbers, formatting chapter headings,
cleaning embedded notes, and so on.
}
\keyword{Preparation}
\keyword{Text}
