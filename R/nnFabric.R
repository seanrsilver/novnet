#' nnFabric()
#'
#' Creates a "Fabric" visualization from a novel, where:
#' Warp yarns (horizontal) are character appearances by chapter
#' Weft yarns connect appearances by name mentions within a context field
#'
#' @param argument Description ends with .
#' @keywords NovNet Utilities
#'
#' @import ggplot2
#' @import tm
#' @import RColorBrewer
#' @import googledrive
#'
#' @export
#'
#'


nnFabric <- function(filename,
                   context = 10,
                   set.voice = NA,
                   chunk.length = 5000,
                   include.all = TRUE,
                   brewer.palette = "YlGnBu", # display.brewer.all()
                   mirror.colors = FALSE,
                   bg.col = "white",
                   thread.edge = "black",
                   thread.width = 1,
                   knot.size = 2,
                   local = FALSE,
                   characters = NA,
                   token.thresh = 1){
  ### 1) Scan in .txt and .csv files, return as data frame

  novel.dat <- Txt_to_df(filename = filename, 
                         set.voice = set.voice, 
                         local = local,
                         include.all = include.all,
                         chunk.length = chunk.length)
  
  ## eliminate characters with token counts less than token.count
  novel.dat$char.data.df <- novel.dat$char.data.df[
    which(novel.dat$char.data.df$tokenCount >= token.thresh), ]
  # and reset novel.dat$dramatis.personae.v
  novel.dat$dramatis.personae.v <- novel.dat$char.data.df$UniqueName
  ### 2) Create the warp yarns ----
  ### The ggplot2:: geom_segment requires 4 values per segment, but 2 can be derived
  ###   from the other two.  We will require:
  ###   a) the chapter numbers for each character appearance
  ###   b) a height (y-value) for each character's warp yarn
  ### This must be in a data.frame with $chapter $character $warpY

  ## Find the chapter appearances for each character and save in a data.frame
  ## Create a matrix, where each col is a character, and row is a chapter

  # pass a vector to a tapply() to pull values from a list and return as matrix
  char.chap.m <- sapply(novel.dat$table.chaps.l, "[", novel.dat$dramatis.personae.v)
  rownames(char.chap.m) <- novel.dat$dramatis.personae.v # adds names to the matrix
  colnames(char.chap.m) <- 1:length(novel.dat$table.chaps.l)
  char.chap.m[is.na(char.chap.m)] <- 0 # replace NA with 0
  char.chap.df <- as.data.frame(char.chap.m) # transform to data frame

  warp.l <- list()
  for(i in 1:ncol(char.chap.df)){
    warp.l[[i]] <- novel.dat$dramatis.personae.v[which(char.chap.df[, i] > 0)]
  }

  warp.m <- NULL
  for(i in 1:length(warp.l)){
    for(j in 1:length(warp.l[[i]])){
      temp.row.v <- c(i, warp.l[[i]][j])
      warp.m <- rbind(warp.m, temp.row.v)
    }
  }

  rownames(warp.m) <- 1:nrow(warp.m)
  warp.df <- data.frame(as.numeric(warp.m[, 1]), as.character(warp.m[, 2]), stringsAsFactors = FALSE)
  colnames(warp.df) <- c("chapter", "character")

  ## create a set of values for the heights of the warp yarns
  # generate a sequence of heights from 0 to 1 equivalent to the number of characters
  char.heights.v <- seq(from = 0, to = 1, along.with = novel.dat$dramatis.personae.v)

  # add character names to the index of the height vector
  names(char.heights.v) <- novel.dat$dramatis.personae.v
  # create a new column in the data.frame indicating the heights of the warp yarns
  warp.df$yarnY <- char.heights.v[warp.df$character]

  ## create values for x and x-end, which are the chapter word counts (including headings)
  chap.breaks.v <- c(grep("CHAPTER", novel.dat$text.uniq.v),
                     length(novel.dat$text.uniq.v)+1)
  if(length(chap.breaks.v) == 1){
    chap.breaks.v <- c(seq(from = 1, to = length(novel.dat$text.uniq.v), by = chunk.length),
                       length(novel.dat$text.uniq.v)+1)
  }
  warp.df$yarnX <- chap.breaks.v[warp.df$chapter]
  warp.df$yarnXEnd <- chap.breaks.v[warp.df$chapter+1]-1

  ## Add a count to the warp yarns-- character's # of appearances by chapter
  ## pull from char.chap.df

  for(i in 1:nrow(warp.df)){
    char.temp.name.v <- warp.df$character[i]
    chap.num.temp.v <- warp.df$chapter[i]
    warp.df$count[i] <- char.chap.df[char.temp.name.v, chap.num.temp.v]
  }

  ### 3) Create the weft yarns ----
  ###   a) Identify everywhere a character name appears within a certain range
  ###       of another character name, and record in an edgelist: from, to, position.
  ##        Position == word number of from character

  ## start by creating a matrix lookforward KWIC

  edge.from.l <- NULL
  for(j in 1:length(novel.dat$dramatis.personae.v)){
    char.temp.v <- novel.dat$dramatis.personae.v[j]
    char.pos.temp.v <- which(novel.dat$text.uniq.v == char.temp.v)
    char.edgelist.l <- NULL
    for(k in 1:length(char.pos.temp.v)){
      temp.pos.v <- char.pos.temp.v[k]
      row.temp.v <- c(novel.dat$text.uniq.v[(temp.pos.v+1):(temp.pos.v+context)])
      edge.from.l[[as.character(temp.pos.v)]] <- row.temp.v # stores position as list element name
    }
  }
  # transform to a table to quickly identify candidates
  table.edge.l <- lapply(edge.from.l, table)
  lapply.table.edge.l <- lapply(table.edge.l, "[", novel.dat$dramatis.personae.v)
  for(i in 1:length(lapply.table.edge.l)){
    lapply.table.edge.l[[i]][is.na(lapply.table.edge.l[[i]])] <- 0
    names(lapply.table.edge.l[[i]]) <- novel.dat$dramatis.personae.v
  }
  # find which names are followed by other names w/in the lookahead context field
  sum.table.edge.v <- sapply(lapply.table.edge.l, sum)
  from.pos.v <- names(which(sum.table.edge.v > 0))
  # create a directional edgelist
  edge.l <- list()
  edge.num.v <- 1
  for(i in 1:length(from.pos.v)){
    to.field.v <- lapply.table.edge.l[[from.pos.v[i]]]
    to.temp.v <- names(which(to.field.v > 0))
    for(j in 1:length(to.temp.v)){ # solves the problem if one lookahead context field has
      # more than one "to" node
      edge.v <- c(from.pos.v[i], novel.dat$text.uniq.v[as.numeric(from.pos.v[i])], to.temp.v[j])
      edge.l[[edge.num.v]] <- edge.v
      edge.num.v <- edge.num.v + 1
    }
  }
  edge.m <- do.call(rbind, edge.l)
  edge.df <- as.data.frame(edge.m, stringsAsFactors = FALSE)
  colnames(edge.df) <- c("position", "from", "to")
  edge.df$position <- as.numeric(edge.df$position)


  ## Add chapter number to positions
  # Find the greatest chapter break which is less than the weft thread position
  for(i in 1:length(edge.df$position)){
    temp.chapter.v <- max(which(chap.breaks.v <= edge.df$position[i]))
    edge.df$chapter[i] <- temp.chapter.v
  }

  ## create weft.df from edge.df
  weft.df <- edge.df
  ## Add $fromX and toX (== position)
  weft.df$toX <- weft.df$fromX <- weft.df$position
  ## Add weftY and weftYEnd
  weft.df$fromY <- char.heights.v[weft.df$from]
  weft.df$toY <- char.heights.v[weft.df$to]

  #plot(x = 1:length(novel.dat$dramatis.personae.v),
  #     y = rep(0, length(novel.dat$dramatis.personae.v)),
  #     pch = 19, col = colramp.mirror.pal(length(novel.dat$dramatis.personae.v))) # test!

  ## Prepare custom factors for color assignment in warp.df$character and weft.df$from
  warp.df$character <- factor(warp.df$character,
                              levels = novel.dat$dramatis.personae.v,
                              ordered = TRUE)
  weft.df <- transform(weft.df,
                       from = factor(from,
                                     levels = novel.dat$dramatis.personae.v,
                                     ordered = TRUE))
  ## ggplot accepts a named vector for colors, where names correspond
  # to factors and values to colors
  ## Create a custom palette
  colramp.pal <- colorRampPalette(brewer.pal(9, name = brewer.palette))
  if(mirror.colors == TRUE){
    colramp.mirror.pal <- function(x){
      c(colramp.pal(x/2)[(x/2):1],
        colramp.pal(x/2))
      }
    colramp.pal <- colramp.mirror.pal
  }
  # and generate vector, bind to .df
  plot.colors.v <- rev(colramp.pal(length(novel.dat$dramatis.personae.v)))
  # this generates a vector of colors in a spectrum, ordered according to the
  # order of plot.df, which is by date of composition
  names(plot.colors.v) <- novel.dat$dramatis.personae.v

  # pick out one or more warp threads:
  char.iso.v <- novel.dat$dramatis.personae.v # replace with c(char1, char2, ...), else dramatis.personae.v
  if(!is.na(characters[1])){
    char.iso.v <- characters
  }
  wa.df <- data.frame()
  we.df <- data.frame()
  for(character.v in char.iso.v){
    we.temp.df <- weft.df[which(weft.df$from == character.v), ]
    wa.temp.df <- warp.df[which(warp.df$character == character.v), ]
    we.df <- rbind(we.df, we.temp.df)
    wa.df <- rbind(wa.df, wa.temp.df)
  }

  ### PLOT!
  ggplot() +
    geom_segment(aes(x = rep(0, length(char.heights.v)), xend = rep(max(chap.breaks.v), length(char.heights.v)),
                     y = char.heights.v, yend = char.heights.v),
                 linetype = 1, size = .1, color = "gray") +
    geom_segment(data = we.df,
                 aes(x = fromX, xend = toX,
                     y = fromY, yend = toY),
                 linetype = 1, size = 1.6*line.width, alpha = 1, color = thread.edge) +
    geom_segment(data = we.df,
                 aes(x = fromX, xend = toX,
                     y = fromY, yend = toY, color = from),
                 linetype = 1, size = 1.2*line.width, alpha = 1) +
    geom_segment(data = wa.df,
                 aes(x = yarnX, xend = yarnXEnd,
                     y = yarnY, yend = yarnY),
                 linetype = 1, size = 1.6*line.width, lineend = "round", color = thread.edge) +
    geom_segment(data = wa.df,
                 aes(x = yarnX, xend = yarnXEnd,
                     y = yarnY, yend = yarnY, color = character),
                 linetype = 1, size = 1.2*line.width, lineend = "round") +
    geom_point(data = we.df,
               aes(x = fromX, y = fromY, fill = from),
               shape = 21, alpha = 1,  size = 2*knot.size, color = thread.edge) +
    geom_point(data = we.df,
               aes(x = toX, y = toY, fill = from),
               shape = 21, alpha = 1,  size = 2*knot.size, color = thread.edge) +
    scale_shape_identity() +
    scale_color_manual(values = plot.colors.v) +
    scale_fill_manual(values = plot.colors.v) +
    ylab(NULL) +
    scale_y_continuous(breaks = char.heights.v,
                       labels = novel.dat$dramatis.personae.v) + # how to add names here?
    xlab("novel time") +
    ggtitle(paste("The Fabric of ", filename)) +
    theme(legend.position = "none") + # removes legend (wch generates error anyways)
    theme(panel.background = element_rect(fill = bg.col),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

}



