#' nnNetwork()
#'
#' A visualization function, which produces a network-style visualization from
#' a properly formed Novel Networks data set.
#' nnNetwork() chains
#'
#' @param filename <- "Frankenstein"
#' @param include.all <- TRUE
#' @param set.voice <- NA
#' @param local <- TRUE

#' @param filename Character string of text name, with associated Char.csv and .txt files.
#' @param include.all Logical vector or numerical range. If TRUE (default), processes whole text.  If range (i.e. 1:18), processes only those chapters.  If FALSE, will offer a prompt to determine a range.
#' @param set.voice NA, or character string. If NA, Char.csv pulls voice from associated *Char.csv file. If "third", will ignore first-person pronouns.  If "first", will attempt to transform all non-dialogue "I" and "me" into narrator, signaled on Char.csv file as "TextEgo".
#' @param local Logical vector.  If FALSE (default), looks in Google Drive for files.  If TRUE, looks for filename in a folder with path data/filename/.
#' @param layout.with Character.  Sets layout algorithm.  Accepts "fr" (Fruchterman-Reingold, default), "gem" (GEM force-directed), "kk" (Kamada-Kawai), "tree", "circle", "sphere", "lgl" (Large Graph).
#' @param plot.community NA for no community detection, "spinglass" for simulated annealing, or "walktrap" for a random walk.
#' @param vertex.size Numeric (default = 1).  Adjusts vertex size in plot.  Coefficient applied to square root of vertex degree.
#' @param edge.width Numeric (default = .5).  Adjusts edge width in plot.  Coefficient applied to square root of total number of context coappearances.
#' @param context Counting number.  How many words from each name should the function seek others?  Default = 15.
#' @param multiplex Logical.  Return a multiplex graph?  Default = FALSE.
#' @param token.threshold Numeric.  Removes characters with token count < x from graph.  Default = 1.
#' @param metadata.off Default = NA. Expects a length-two vector of c("meta", "VALUE"), or matrix of vectors, to cull characters by metadata category.  Useful for removing non-characters, generating subgraphs by gender, etc...
#' @param sample Default = FALSE.  Logical, indicating whether the .txt should be randomized (i.e. words scrambled).  Useful for checking whether modularity results are statistically meaningful.
#' @param vertex.label.thresh Numeric (Default = 1). Only vertices >= this value will be labeled.
#' @param bg.col Character. Sets background color for plot. Default = "white".
#'
#' @keywords NovNet Utilities
#'
#' @import igraph
#'
#' @export
#'

nnNetwork <- function(filename,
                      include.all = TRUE,
                      set.voice = NA,
                      local = FALSE,
                      context = 15,
                      layout.with = "fr",
                      plot.community = "spinglass",
                      vertex.size = 1,
                      edge.width = .5,
                      multiplex = FALSE,
                      token.threshold = 1,
                      metadata.off = NA,
                      sample = FALSE,
                      vertex.label.thresh = 1,
                      bg.col = "white"){

  ### 1) Scan in .txt and .csv files, return as data frame ----
  novel.dat <- Txt_to_df(filename = filename,
                         include.all = TRUE,
                         set.voice = NA, # set to override Char.csv settings
                         local = FALSE)

  ### 2) Generate edgelist from char.data.df and text.uniq.v ----
  ###   a) Identify everywhere a character name appears within a certain range
  ###       of another character name, and record in an edgelist: from, to, position.
  ##        Position == word number of from character

  ## create a lookforward KWIC matrix
  novel.net <- charDf_to_edgelist(data = novel.dat,
                                  context = 15,
                                  multiplex = FALSE,
                                  token.threshold = 1, # Austen, 10
                                  metadata.off = NA, # for Austen settings: c("character, "N")
                                  sample = FALSE)

  ## Extract a network object (net.igr)
  net.igr <- nnLayout(novel.net = novel.net,
                      layout.with = "fr",
                      remove.isolates = TRUE,
                      min.degree = 1)

  ## Create Colors
  E(net.igr)$edge.color <- createColorPalette(edgelist = net.igr,
                                              vertex.color.index = "weight",
                                              color.palette = c("gray80", "gray20"),
                                              alpha.range = c(.2, .8),
                                              vertex.edge = "edge")
  V(net.igr)$vertex.color <- createColorPalette(edgelist = net.igr,
                                                edge.color.index = "tokenCount",
                                                color.palette = c("yellow", "tomato"),
                                                alpha.range = c(.5, 1),
                                                vertex.edge = "vertex")

  # Cut off names of low-degree characters
  V(net.igr)$vertex.label <- V(net.igr)$name
  V(net.igr)$vertex.label[which(V(net.igr)$tokenCount <= vertex.label.thresh)] <- NA

  ## Plot
  # Set plot parameters
  # background color and margins
  par(bg = bg.col, mar = c(1, 1, 1, 1))
  # plot title
  if(plot.community != FALSE){title.start.v <- "The Communities of "} else {
    title.start.v <- "Character Network of "}
  main.title <- paste0(title.start.v, " ",
                       novel.dat$file.metadata.v["Title"],
                       " (",
                       novel.dat$file.metadata.v["Year"],
                       ")")
  # Plot without community:
  if(plot.community == FALSE){
    plot(net.igr,
         edge.arrow.size = 0, edge.curved = 0,
         edge.width = edge.width*E(net.igr)$weight^.4,
         edge.color = E(net.igr)$edge.color,
         layout = net.igr$layout,
         vertex.size = vertex.size*degree(net.igr)^.4,
         vertex.color = V(net.igr)$vertex.color,
         vertex.label = V(net.igr)$vertex.label,
         vertex.label.family = "Monaco", vertex.label.color = "black",
         vertex.label.cex = .5,
         label.degree = pi/2,
         label.dist = 1,
         main = main.title)
  }
  # Plot with community detection
  # generate community object (net.com) from walktrap or spinglass algorithms
  if(plot.community == "walktrap"){
    # walktrap
    net.com <- cluster_walktrap(net.igr)
  }
  if(plot.community == "spinglass"){
    # spinglass (involves finding the largest connected component)
    max.component.which.v <- which.max(clusters(net.igr)$csize)
    max.component.v <- which(clusters(net.igr)$membership == max.component.which.v)
    subgraph.igr <- induced.subgraph(net.igr, max.component.v)
    net.com <- cluster_spinglass(subgraph.igr)
    # map communities from subgraph to net.igr for color assignment
    V(net.igr)$community.from.subgraph <-
      membership(net.com)[V(net.igr)$name]
  }
  if(!is.na(plot.community)){
    ## assign color functions-- two options (one for gradient, the other for communities)
    colors.fun <- colorRampPalette(brewer.pal(9, "Blues"))
    default.colors <- categorical_pal(max(membership(net.com)))

    plot(net.com,
         net.igr,
         mark.col = NA,
         mark.border = "gray80",
         col = default.colors[V(net.igr)$community.from.subgraph],
         edge.arrow.size = 0, edge.curved = 0,
         edge.width = E(net.igr)$weight^.4,
         edge.color = E(net.igr)$edge.color,
         layout = net.igr$layout,
         vertex.size = vertex.size*degree(net.igr)^.4,
         vertex.label = V(net.igr)$vertex.label,
         vertex.label.family = "Monaco",
         vertex.label.color = "black",
         vertex.label.cex = .8,
         label.degree = pi/2,
         label.dist = 1,
         main = main.title)
  }
}
