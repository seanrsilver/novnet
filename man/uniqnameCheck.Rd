% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uniqnameCheck.R
\name{uniqnameCheck}
\alias{uniqnameCheck}
\title{uniqnameCheck()}
\usage{
uniqnameCheck(filename, local = FALSE)
}
\arguments{
\item{filename}{File name as character string, i.e. "Crusoe".}

\item{local}{Default = TRUE. If FALSE, searches for file in google drive. If TRUE, seeks file in folder filename/.}
}
\description{
Checks for duplicate rownames in Char.csv uniqname, and returns a matrix
Run if txtDisambig fails with "duplicate row names not allowed"
}
\keyword{Preparation}
\keyword{Text}
