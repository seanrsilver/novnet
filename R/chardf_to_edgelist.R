### charDF_to_edgelist


#' charDF_to_edgelist()
#'
#' Along with txt_to_DF(), this is one of the crucial NovNet utilities.
#' This function accepts objects from Txt_to_df()
#' It requires
#'   a) char.data.df
#'   b) text.uniq.v
#' And returns:
#'   1) an edgelist as a dataframe
#'   2) an igraph object
#'
#' @param data A data object of the format returned by txt_to_Df().  Default = novel.dat.
#' @param context Counting number.  How many words from each name should the function seek others?  Default = 15.
#' @param multiplex Logical vector of length one.  Return a multiplex graph?  Default = FALSE.
#' @param token.threshold Length-one numerical vector.  Removes characters with token count < x from graph.  Default = 1.
#' @param metadata.off Default = NA. Expects a length-two vector of c("meta", "VALUE"), or matrix of vectors, to cull characters by metadata category.  Useful for removing non-characters, generating subgraphs by gender, etc...
#' @param sample Default = FALSE.  A logical vector indicating whether the .txt should be randomized (i.e. words scrambled).  Useful for checking whether modularity results are statistically meaningful.
#' @param noisy Defaults to TRUE.
#'
#' @keywords NovNet Utilities
#'
#' @import igraph
#'
#' @export

### 2) create edgelist
### NOTE: this creates as default context field if context does not already exist
charDf_to_edgelist <- function(data = novel.dat,
                               context = 15,
                               multiplex = FALSE,
                               token.threshold = 1, #
                               metadata.off = NA, # vector of c("meta", "VALUE"), or matrix of vectors, to cull characters by metadata
                               sample = FALSE,
                               noisy = TRUE){

  # if sample == TRUE then use sample.uniq.v instead of text.uniq.v,
  #   thereby generating a graph from a randomized word order
  if(sample == TRUE){
    data$text.uniq.v <- sample(data$text.uniq.v)
  }

  # trim data$char.data.df by applying token.threshold to $tokenCount
  rows.tokenthresh.v <- which(data$char.data.df$tokenCount >= token.threshold)
  data$char.data.df <- data$char.data.df[rows.tokenthresh.v, ]
  # trim data$char.data.df by metadata.off
  metadata.off <- rbind(metadata.off) # if vector, converts to 1-row matrix
  if(!is.na(metadata.off)[1]){
    for(i in 1:nrow(metadata.off)){
      meta.leave.in.v <- which(data$char.data.df[metadata.off[i, 1]] != metadata.off[i, 2])
      data$char.data.df <- data$char.data.df[meta.leave.in.v, ]
    }
  }
  # create context variable if it doesn't exist
  if(!exists("context")){
    context <- 15
    cat("context field set to default (15)\n")
  }
  if(noisy == TRUE){
    cat("generating edgelist with context = ", context, "\n")
  }
  edge.from.l <- NULL
  absent.characters <- NULL
  # pull list of characters
  for(j in 1:length(data$char.data.df$UniqueName)){
    char.temp.v <- data$char.data.df$UniqueName[j]
    char.pos.temp.v <- which(data$text.uniq.v == char.temp.v)
    if(length(char.pos.temp.v) > 0){
      char.edgelist.l <- NULL
      for(k in 1:length(char.pos.temp.v)){
        temp.pos.v <- char.pos.temp.v[k]
        row.temp.v <- data$text.uniq.v[(temp.pos.v+1):(temp.pos.v+context)]
        edge.from.l[[as.character(temp.pos.v)]] <- row.temp.v # stores position as list element name
      }
    } else {
      absent.characters <- c(absent.characters, char.temp.v)
    }
  }
  if(noisy == TRUE){
    if(length(absent.characters) > 0){
    cat("NOTE: ", length(absent.characters), "characters were not found.\n",
        "Call absent.characters to see a vector of character names.\n",
        "----------------------------------------------------------\n")
    }
  }
  # transform to a table to quickly identify candidates
  table.edge.l <- lapply(edge.from.l, table)
  lapply.table.edge.l <- lapply(table.edge.l, "[", data$char.data.df$UniqueName)
  for(i in 1:length(lapply.table.edge.l)){
    lapply.table.edge.l[[i]][is.na(lapply.table.edge.l[[i]])] <- 0
    names(lapply.table.edge.l[[i]]) <- data$char.data.df$UniqueName
  }
  # find which names are followed by other names w/in the lookahead context field
  sum.table.edge.v <- sapply(lapply.table.edge.l, sum)
  from.pos.v <- names(which(sum.table.edge.v > 0))
  # create a directional edgelist, store as edge.df
  {edge.l <- list()
    edge.num.v <- 1
    for(i in 1:length(from.pos.v)){
      to.field.v <- lapply.table.edge.l[[from.pos.v[i]]]
      to.temp.v <- names(which(to.field.v > 0))
      for(j in 1:length(to.temp.v)){ # solves the problem if one lookahead context field has
        # more than one "to" node
        edge.v <- c(data$text.uniq.v[as.numeric(from.pos.v[i])], to.temp.v[j], from.pos.v[i])
        edge.l[[edge.num.v]] <- edge.v
        edge.num.v <- edge.num.v + 1
      }
    }
    edge.m <- do.call(rbind, edge.l)
    edge.df <- as.data.frame(edge.m, stringsAsFactors = FALSE)
    colnames(edge.df) <- c("from", "to", "position")
    edge.df$position <- as.numeric(edge.df$position)
    edge.df <- edge.df[order(edge.df$position), ]
  }
  # Create vector of metadata categories, from data$char.data.df
  metadata.cats <- setdiff(colnames(data$char.data.df),
                             grep("CharName|Alt\\d+|NOTES|tokenCount",
                                  colnames(data$char.data.df), value = TRUE))

  ### 3) Create an igraph network object (.igr) ----
  ### Note: igraph will "match" a vector of vertex names to a data.frame of edges
  ### Note: this will produce a multiplex graph-- where each instance in text.uniq.v
  ###    will be plotted.  To collapse these into a single, weighted edge,
  ###    use simplify(remove.multiple = TRUE, edge.attr.comb = list(weight = "sum", "ignore"))

  novel.igr <- graph_from_data_frame(d = edge.df,
                                     vertices = cbind(rownames(data$char.data.df), data$char.data.df),
                                     directed = TRUE)

  # Clean up multiplex edges, if(multiplex == FALSE)
  if(multiplex == FALSE){
    E(novel.igr)$weight <- 1 # assigns uniform weight to multiplex edges
  }


  chardf_to_edgelist.l <- list()

  objects.to.return <- c("edge.df", "absent.characters",
                         "metadata.cats", "novel.igr")
  for(l in 1:length(objects.to.return)){
    object.name <- objects.to.return[l]
    chardf_to_edgelist.l[[object.name]] <- get(object.name)
  }
  if(noisy == TRUE){
    cat("Returned a list containing:
      1) $edge.df, an edgelist
      2) $absent.characters, a vector of characters in *Char.csv but not *.txt
      3) $metadata.cats, a vector of metadata categories appearing in *Char.csv
      4) $novel.igr, an igraph network object, with properties:
         Vertex: name (v/c), charName (v/c), tokenCount (v/n)
         Alt1:n (v/c), metadata (v/c)
         Edge: position (e/n), weight <- 1 (e/n)\n")
  }
  chardf_to_edgelist.l
}





