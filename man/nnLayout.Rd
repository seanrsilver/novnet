% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nnLayout.R
\name{nnLayout}
\alias{nnLayout}
\title{Network()
This function bundles together steps following charDf_to_edgelist().}
\usage{
nnLayout(novel.net = novel.net, layout.with = "fr",
  remove.isolates = TRUE, min.degree = 0)
}
\arguments{
\item{novel.net}{List including igraph network, produced by charDf_to_edgelist().  Default is novel.net.}

\item{layout.with}{FALSE, or Character. Determines igraph layout function to apply. Accepts "fr" (layout_with_fr, default), "gem" (layout_with_gem), "kk", "tree", "circle", "sphere", "lgl". If FALSE, no layout is attached.}

\item{remove.isolates}{Logical.  Passed to chardf_to_edgelist.  If TRUE, isolates are removed}

\item{min.degree}{Numeric.  Vertices of lower degree are ignored.}
}
\description{
It requires the list returned by charDf_to_edgelist (novel.net).
It simplifies, removes isolates, and attaches a layout as a global variable
}
\keyword{NovNet}
\keyword{Utilities}
