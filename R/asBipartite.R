#' asBipartite()
#'
#' asBipartite() splits an edgelist into a strictly bipartite graph around
#'  a specified metadata category
#'  requires novel.net$edge.df and net.igr from Network()
#'   Steps:
#'   1) divide character uniqnames around metadata category
#'      using net.igr
#'   2) create sub-edgelist of only bipartite edges from edge.df
#'      this step will find edges with exactly one element from
#'      one of the metadata categories
#'   3) outputs bipartite igraph
#'
#' @param net.igr An igraph network.
#' @param part.by A metadata category (i.e. gender) by which to split the network into a strictly bipartite graph.
#' @param summary.table Default = TRUE. Returns a data frame, summarizing edges across the bipartite graph.
#' @param vertex.colors Default = NA. A length-two vector of colors. Default == c("Gold", "Tomato").
#' @keywords Bipartite Graph Utilities
#'
#' @import igraph
#'
#' @export

asBipartite <- function(net.igr = net.igr,
                        part.by = NA,
                        summary.table = TRUE,
                        vertex.colors = NA){
  ## set metadata category by which to partition
  ## NOTE: metadata category will sort by option 1 and not-option-1
  ##       in order to produce bipartite graph (i.e. of two exclusive categories)
  if(is.na(part.by) == TRUE){
    if(exists("novel.net")){
      print(novel.net$metadata.cats)
    } else {
      print(list.vertex.attributes(net.igr))
    }
    part.by <- as.character(readline("Enter a metadata category for partitioning (without quotes): "))
  }

  vertex.meta.vals.v <- get.vertex.attribute(net.igr, part.by)
  meta.atts.v <- unique(vertex.meta.vals.v)

  bipartite.set1.v <- V(net.igr)$name[which(vertex.meta.vals.v == meta.atts.v[1])]
  bipartite.set2.v <- setdiff(V(net.igr)$name, bipartite.set1.v)

  ### 2) create sub-edgelist of only bipartite edges from edge.df
  ###    this step will find edges with exactly one element from
  ###    one of the metadata categories

  from.to.m <- cbind(novel.net$edge.df$from, novel.net$edge.df$to)
  # create and stuff from.to.set1.m with which( %in% )
  #     to find places where bipartite.set1.v appears in from.to.m
  from.to.set1.m <- matrix(nrow = nrow(from.to.m), ncol = 2)
  from.to.set1.m[which(from.to.m %in% bipartite.set1.v)] <- from.to.m[which(from.to.m %in% bipartite.set1.v)]

  # find rows with unequal values (i.e. NA and !NA)
  bipartite.rows.v <- which(is.na(from.to.set1.m[, 1]) != is.na(from.to.set1.m[, 2]))
  unipartite.rows.v <- which(is.na(from.to.set1.m[, 1]) == is.na(from.to.set1.m[, 2]))
  ### 3) create a bipartite graph, using only the edges established in part 2)
  novel.subnet <- novel.net
  novel.subnet$edge.df <- novel.net$edge.df[bipartite.rows.v, ]
  novel.subnet$novel.igr <- delete.edges(novel.subnet$novel.igr, unipartite.rows.v)

  subnet.igr <- as.undirected(Network(novel.net = novel.subnet, layout = "kk"))
  V(subnet.igr)$type <- get.vertex.attribute(subnet.igr, name = "Character.Non") == meta.atts.v[1]

  bipartite.layout <- layout_as_bipartite(as.undirected(subnet.igr))
  subnet.igr$layout <- bipartite.layout[ ,c(2,1)] # rows to columns
  ## Create colors
  if(is.na(vertex.colors) == TRUE){
    vertex.colors <- c("Gold", "Tomato")
  }
  V(subnet.igr)$vertex.color <- rep(vertex.colors[1], length(V(subnet.igr)))
  V(subnet.igr)$vertex.color[which(get.vertex.attribute(subnet.igr, name = part.by) == meta.atts.v[1])] <- vertex.colors[2]

  if(summary.table == TRUE){
    ## Return a matrix of preliminary results, indicating the
    ## Which C is associated with the most N (and v-v)?
    rel.vert.degree.v <- 100*degree(subnet.igr)/V(subnet.igr)$tokenCount # degree divided by token count
    # bind token count to separate column, and print to screen
    Set1toSet2.df <- data.frame("Frequency" = round(as.numeric(rel.vert.degree.v), digits = 3),
                      "Tokens" = as.numeric(get.vertex.attribute(subnet.igr, "tokenCount")),
                      "Set" = as.character(get.vertex.attribute(subnet.igr, part.by)),
                      row.names = names(rel.vert.degree.v))
    Set1toSet2.df <- Set1toSet2.df[order(Set1toSet2.df[, 1], decreasing = TRUE), ]
    print(Set1toSet2.df)
  }

  subnet.igr
}
