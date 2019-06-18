#' Network()
#' This function bundles together steps following charDf_to_edgelist().
#'
#' It requires the list returned by charDf_to_edgelist (novel.net).
#' It simplifies, removes isolates, and attaches a layout as a global variable
#'
#'
#' @param novel.net List including igraph network, produced by charDf_to_edgelist().  Default is novel.net.
#' @param layout.with FALSE, or Character. Determines igraph layout function to apply. Accepts "fr" (layout_with_fr, default), "gem" (layout_with_gem), "kk", "tree", "circle", "sphere", "lgl". If FALSE, no layout is attached.
#' @param remove.isolates Logical.  Passed to chardf_to_edgelist.  If TRUE, isolates are removed
#' @param min.degree Numeric.  Vertices of lower degree are ignored.
#'
#' @keywords NovNet Utilities
#'
#' @import igraph
#' @import ggplot2
#'
#' @export

nnLayout <- function(novel.net = novel.net,
                    layout.with = "fr",
                    remove.isolates = TRUE,
                    min.degree = 0){
  ### Create a network object (net.igr) from the more flexible novel.igr ----

  net.igr <- simplify(novel.net$novel.igr, remove.loops = TRUE,
                      remove.multiple = TRUE,
                      edge.attr.comb = list(weight = "sum", "ignore"))

  ## A function to remove isolates and low-degree vertices
  removeIsolates <- function(data = net.igr, degree.v = 0){
    iso.igr <- V(net.igr)[degree(net.igr) <= degree.v]
    delete.vertices(net.igr, iso.igr)
  }
  if(remove.isolates == TRUE){
    net.igr <- removeIsolates(data = net.igr, degree.v = min.degree)
  }

  # standardize layout
  if(layout.with == "fr"){
    net.igr$layout <- layout_with_fr(net.igr)
  }
  if(layout.with == "gem"){
    net.igr$layout <- layout_with_gem(net.igr)
  }
  if(layout.with == "kk"){
    net.igr$layout <- layout_with_kk(net.igr)
  }
  if(layout.with == "tree"){
    net.igr$layout <- layout_as_tree(net.igr)
  }
  if(layout.with == "circle"){
    net.igr$layout <- layout_in_circle(net.igr)
  }
  if(layout.with == "sphere"){
    net.igr$layout <- layout_on_sphere(net.igr)
  }
  if(layout.with == "lgl"){
    net.igr$layout <- layout_with_lgl(net.igr)
  }

  net.igr
}






