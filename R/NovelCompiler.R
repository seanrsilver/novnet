#' compDegreeDist()
#'
#' This script generates .igr objects for all the novels stored locally
#' And enables comparative analyses
#' It outputs a list of lists (of lists) of the format
#' novels.l$filename
#' each $filename contains: 1) novel.igr
#'                          2) Txt_to_df.l
#'                          3) charDf_to_edgelist.l
#'
#' @param token.threshold Counting number, passed to charDf_to_edgelist. Removes characters of token-count less than value.  Default = 1.
#' @keywords NovNet Utilities
#'
#' @import googledrive
#' @import igraph
#'
#' @export

NovelCompiler <- function(token.threshold = 1){
  ### 1) generate a list of folder names, file names
  novel.dirs.v <- list.dirs(path = "data")[-1] # removes the base /data/ directory
  file.names.v <- gsub("data/", "", novel.dirs.v) # generate file names by removing "data/"
  # generate a vector of directories that contain both *.txt and *Char.csv files
  novels.v <- NULL
  for(i in 1:length(novel.dirs.v)){
    directory.temp <- novel.dirs.v[i]
    file.temp <- file.names.v[i]
    if(file.exists(paste0(directory.temp, "/", file.temp, ".txt")) &&
       file.exists(paste0(directory.temp, "/", file.temp, "Char.csv"))){
      novels.v <- c(novels.v, file.temp)
    }
  }

  ### 2) populate a list with the network objects of novels.v
  novels.raw.l <- list()
  for(j in 1:length(novels.v)){
    novel.temp <- novels.v[j]

    ## create a character and text data object (.dat)
    Txt_to_df.l <- Txt_to_df(filename = novel.temp,
                             local = TRUE,
                             set.voice = "third",
                             include.all = TRUE)

    ## create a network and edgelist object (.net)
    charDf_to_edgelist.l <- charDf_to_edgelist(data = Txt_to_df.l,
                                               context = 15,
                                               multiplex = FALSE,
                                               token.threshold = token.threshold,
                                               metadata.off = NA)

    ## create a network object (net.igr) from the more flexible novel.igr ----

    net.igr <- simplify(charDf_to_edgelist.l$novel.igr, remove.loops = TRUE,
                        remove.multiple = TRUE,
                        edge.attr.comb = list(weight = "sum", "ignore"))

    ## populate the list
    novels.raw.l[[novel.temp]][["net.igr"]] <- net.igr
    novels.raw.l[[novel.temp]][["Txt_to_df.l"]] <- Txt_to_df.l
    novels.raw.l[[novel.temp]][["charDf_to_edgelist.l"]] <- charDf_to_edgelist.l
  }

  ### 3) sort the list by sort argument, from file metadata
  novel.dates.v <- NULL
  for(k in 1:length(novels.raw.l)){
    novel.dates.v[k] <- novels.raw.l[[k]]$Txt_to_df.l$file.metadata.v["Year"]
  }
  novels.l <- novels.raw.l[order(novel.dates.v)]

  ### 4) Output compiled data as list
  novels.l
}


