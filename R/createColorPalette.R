
#' createColorPalette()
#'
#' This function generates edge and vertex colors.
#'
#' @param edgelist Default = net.igr; an igraph-type graph object, from charDf_to_edgelist().
#' @param edge.color.index Character string. Which edge value should be used to assign colors to edges? Defaults to "weight".
#' @param vertex.color.index Character string. Which vertex value should used to assign colors to vertices? Defaults to "tokenCount".
#' @param color.palette Length-two character vector.  Default = c("gold", "tomato").
#' @param alpha.range Length-two numeric vector 0 <= x <= 1. Default = c(.5, 1).  Sets alpha (transparency) for edges.
#' @param vertex.edge Character string "vertex" or "edge".  Return values for edges or vertices?
#'
#' @keywords NovNet Utilities
#'
#' @import igraph
#' @import RColorBrewer
#'
#' @export


##
createColorPalette <- function(edgelist = net.igr,
                               edge.color.index = "weight",
                               vertex.color.index = "tokenCount",
                               color.palette = c("gold", "tomato"),
                               alpha.range = c(.5, 1),
                               vertex.edge = "edge"){
  color.palette.v <- color.palette
  alpha.range.v <- alpha.range # numbers from0 to 1 for transparency of edges
  color.alpha.v <- NULL
  for(i in 1:length(color.palette.v)){
    color.alpha.v[i] <- adjustcolor(color.palette.v[i], alpha.f = alpha.range.v[i])
  }
  paletteRamp <- colorRampPalette(color.alpha.v, alpha = TRUE)
  if(vertex.edge == "edge"){
    edgelist.attr.v <- edge_attr(edgelist, edge.color.index)
    palette.Ramp.v <- paletteRamp(max(edgelist.attr.v))
    output <- palette.Ramp.v[edgelist.attr.v]
  }
  if(vertex.edge == "vertex"){
    vertex.attr.v <- vertex_attr(edgelist, vertex.color.index)
    palette.Ramp.v <- paletteRamp(length(vertex.attr.v))
    output <- palette.Ramp.v[order(vertex.attr.v)]
  }
  output
}

