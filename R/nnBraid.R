#' Braid()
#'
#' This function produces a "braid" visualization of a novel.  The novel is divided into chunks of length = segment.length.  Edges trace characters between chunks.  Specific characters may be highlighted by specifying with the "characters" argument.
#'
#' @param filename Character string of text name, with associated Char.csv and .txt files.
#' @param segment.length Numeric (Default = 1000).  Sets length of each segment in # of words.
#' @param include.all Logical vector or numerical range. If TRUE (default), processes whole text.  If range (i.e. 1:18), processes only those chapters.  If FALSE, will offer a prompt to determine a range.
#' @param characters Character vector.  NA, or 1 or more uniqnames of characters in assocated Char.csv file.  Specified characters are highlighted in plot.
#' @param set.voice NA, or character string. If NA, Char.csv pulls voice from associated *Char.csv file. If "third", will ignore first-person pronouns.  If "first", will attempt to transform all non-dialogue "I" and "me" into narrator, signaled on Char.csv file as "TextEgo".
#' @param local Logical.  If FALSE (default), looks in Google Drive for files.  If TRUE, looks for filename in a folder with path data/filename/.
#' @param legend.position Character location (i.e. "bottomleft" default).  Sets location of legend for spotlighted characters.
#' @param bg.col Color (default = "white").  Sets background color.
#' @param edge.col Color (default = "gray80").  Sets edge color.
#' @param char.only Logical (default = FALSE).  If TRUE, only highlighted characters (specified in "character" argument) are plotted.
#'
#' @keywords NovNet Utilities
#'
#' @import ggplot2
#' @import tm
#' @import igraph
#' @import googledrive
#'
#' @export

nnBraid <- function(filename,
                    segment.length = 1000,
                    include.all = TRUE,
                    local = FALSE,
                    characters = NA,
                    legend.position = "bottomright",
                    bg.col = "white",
                    edge.col = "gray80",
                    char.only = FALSE){

  novel.dat <- Txt_to_df(filename = filename,
                         include.all = include.all,
                         set.voice = NA,
                         local = local)

  ### 2) this for() loop creates a "stitch" list ----
  # Each element of the stitch list is a vector of the segments
  # where given character appears
  # I'm calling it a "stitch" list because it follows a character
  # as she or he threads her/his way through a text

  ## Split text.v into a list of token count tables by slice
  # create vector of breaks
  segment.length.v <- segment.length
  segment.breaks.v <- c(seq(from = 1, to = length(novel.dat$text.uniq.v),
                            by = segment.length.v), length(novel.dat$text.v)+1)
  # use for() loop to store text.uniq.v as list of token count tables
  text.seg.l <- list()
  for(i in 1:(length(segment.breaks.v)-1)){
    start.v <- segment.breaks.v[i]
    end.v <- segment.breaks.v[i+1]-1
    segment.text.v <- novel.dat$text.uniq.v[start.v:end.v]
    text.seg.l[[i]] <- sort(table(segment.text.v), decreasing = TRUE)
  }
  # text.seg.l <- lapply(text.chaps.l, table) ## FOR DIVIDING BY CHAPTERS

  char.seg.m <- matrix(0, nrow = length(text.seg.l),
                       ncol = length(novel.dat$dramatis.personae.v),
                       dimnames = list(1:length(text.seg.l), novel.dat$dramatis.personae.v))

  for(i in 1:length(novel.dat$dramatis.personae.v)){
    char.seg.m[, i] <- sapply(text.seg.l, "[", novel.dat$dramatis.personae.v[i])
  }
  char.seg.m[is.na(char.seg.m)] <- 0

  ## Create the stitch list

  char.stitch.l <- list()
  for(i in 1:ncol(char.seg.m)){
    char.stitch.l[[novel.dat$dramatis.personae.v[i]]] <- as.integer(which(char.seg.m[, i] != 0))
  }
  stitch.edge.m <- NULL
  # this for() loop transforms the "stitch" list into an edgelist
  for(i in 1:length(char.stitch.l)){
    if(length(char.stitch.l[[i]]) >= 2){
      for(j in 1:(length(char.stitch.l[[i]])-1)){
        stitch.temp.v <- c(char.stitch.l[[i]][j], char.stitch.l[[i]][j+1],
                           novel.dat$dramatis.personae.v[[i]])
        stitch.edge.m <- rbind(stitch.edge.m, stitch.temp.v)
      }
      rownames(stitch.edge.m) <- 1:nrow(stitch.edge.m)
      colnames(stitch.edge.m) <- c("from", "to", "character")
    }
  }

  # transform into a data frame, and transform the columns into integer and character vectors
  stitch.edge.df <- as.data.frame(stitch.edge.m, stringsAsFactors = FALSE)
  stitch.edge.df$from <- as.integer(stitch.edge.df$from)
  stitch.edge.df$to <- as.integer(stitch.edge.df$to)

  ## Transform into an igraph object
  stitch.igr <- graph_from_data_frame(d = stitch.edge.df, vertices = 1:nrow(char.seg.m), directed = TRUE)

  # label the first and last nodes

  V(stitch.igr)$segment <- V(stitch.igr)$name
  V(stitch.igr)$segment[1] <- "START"
  V(stitch.igr)$segment[length(V(stitch.igr)$segment)] <- "END"

  # add a color spectrum to the nodes

  #vertex.color.cRP <- colorRampPalette(c("white", "gold", "orange", "tomato", "red3")) # outputs a function
  #plot(stitch.igr,
  #     edge.arrow.size = .1,
  #     vertex.size = 3.75,
  #     layout = layout_with_kk,
  #     vertex.label = V(stitch.igr)$segment,
  #     vertex.label.family = "Helvetica", vertex.label.color = "black",
  #     vertex.color = vertex.color.cRP(length(V(stitch.igr)$segment)))

  # add color edges of given character types, and thicken those edges
  par(bg = bg.col)
  edge.color.v <- edge.col

  #unique(E(stitch.igr)$character) # generates a character list from edge types
  edge.col.v <- rep(edge.color.v, length(E(stitch.igr)$character))
  edge.width.v <- rep(1, length(E(stitch.igr)$character))
  if(!is.na(characters[1])){
    # change the colors and widths of the edges for the selected characters
    for(i in 1:length(characters)){
      edge.col.v[which(E(stitch.igr)$character == characters[i])] <- heat.colors(length(characters))[i]
      edge.width.v[which(E(stitch.igr)$character == characters[i])] <- 3
    }
  }
  E(stitch.igr)$edge.color <- edge.col.v
  E(stitch.igr)$edge.width <- edge.width.v

  ## Community detection
  stitch.comm <- walktrap.community(stitch.igr)
  V(stitch.igr)$community <- stitch.comm$membership
  colors.v <- brewer.pal(12, "Set3")

  # Number the nodes with chapter information for books with chapter
  # else map chunk number to chapter number
  # Find the greatest chapter break which is less than the weft thread position
  chap.breaks.v <- c(grep("CHAPTER", novel.dat$text.uniq.v),
                     length(novel.dat$text.uniq.v)+1)
  seg.chap.num.simple.v <- NULL
  if(length(chap.breaks.v) > 1){
    for(i in 1:(length(segment.breaks.v)-1)){
      segment.center.v <- segment.breaks.v[i] + segment.length.v/2
      temp.chapter.v <- max(which(chap.breaks.v <= segment.center.v))
      seg.chap.num.simple.v[i] <- temp.chapter.v
    }
  } else {
    seg.chap.num.simple.v <- 1:(length(segment.breaks.v)-1)
  }

  seg.chap.num.v <- seg.chap.num.simple.v
  seg.chap.num.v[1] <- "START"
  seg.chap.num.v[length(seg.chap.num.v)] <- "END"
  V(stitch.igr)$chapter <- seg.chap.num.v # adds to network object

  # add START and END
  seg.chap.num.v <- seg.chap.num.simple.v
  seg.chap.num.v[1] <- "START"
  seg.chap.num.v[length(seg.chap.num.v)] <- "END"
  V(stitch.igr)$chapter <- seg.chap.num.v # adds to network object

  E(stitch.igr)$edge.width.plot <- E(stitch.igr)$edge.width
  E(stitch.igr)$edge.color.plot <- E(stitch.igr)$edge.color
  if(char.only == TRUE){  # plot only edges of characters, but retain layout
    E(stitch.igr)$edge.width.plot[which(E(stitch.igr)$edge.width.plot == 1)] <- 0
    E(stitch.igr)$edge.color.plot[which(E(stitch.igr)$edge.color.plot == edge.color.v)] <- bg.col
  }

  plot(stitch.igr,
       edge.arrow.size = .15*E(stitch.igr)$edge.width.plot,
       vertex.size = 7,
       layout = layout_with_kk,
       vertex.label = V(stitch.igr)$chapter,
       vertex.label.family = "Helvetica", vertex.label.color = "black",
       vertex.color = colors.v[V(stitch.igr)$community],
       edge.color = E(stitch.igr)$edge.color.plot,
       edge.width = E(stitch.igr)$edge.width.plot,
       main = paste("Character Braid of ", filename))
  if(!is.na(characters[1])){
    legend(legend.position, characters, pch = 21,
           col = "#777777", pt.bg = heat.colors(length(characters)),
           pt.cex = 5, bty = "n", ncol = 1)
  }
}
