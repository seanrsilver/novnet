#' docSetComplete()
#' pulls all files with .txt and Char.csv
#' runs docSetCheck() for all files
#' writes docSetCheck.csv
#'
#' @param local If FALSE (default), downloads from Google Drive and saves to folder.
#'
#' @keywords NovNet Utilities
#'
#' @export

docSetComplete <- function(local = TRUE){
  ## a) generate vector of titles novels.v
  if(local == FALSE){
    ## generate set of novels by finding Char.csv files on Drive, and verifying .txt
    cat("Compiling directory of files on Drive.\n")
    drive.files.df <- drive_find()
    drive.char.csv.v <- drive.files.df$name[grep("Char.csv", drive.files.df$name)]
    novel.candidates.v <- gsub("Char.csv", "", drive.char.csv.v)
    novels.v <- c()
    for(i in 1:length(novel.candidates.v)){
      novel.txt.v <- paste0(novel.candidates.v[i], ".txt")
      if(novel.txt.v %in% drive.files.df$name){
        novels.v <- c(novels.v, novel.candidates.v[i])
      }
    }
    # remove files with -
    novels.v <- novels.v[-grep("\\-", novels.v)]
  } else {
    cat("Compiling directory of files in data/\n")
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
  }
  novels.v <- novels.v[order(novels.v)]
  ## b) generate blank database to populate with results
  results.df <- data.frame("filename" = character(),
                           "Compiler" = character(),
                           ".txt" = character(),
                           "Char.csv" = character(),
                           "Chap.csv" = character(),
                           "Report.txt" = character(),
                           "Epistle.csv" = character(),
                           "ambiguous.alt" = character(),
                           "orphaned.alt" = character(),
                           "problem.meta" = character(),
                           stringsAsFactors = FALSE)

  ## c) iterate docSetCheck() over all files
  for(k in 1:length(novels.v)){
    filename <- novels.v[k]
    cat(paste0("\nChecking document status for ", filename, ".\n"))

    ## b) pull and transform Document Set, stash in results.df
    result.v <- docSetCheck(filename,
                            return.result = TRUE,
                            local = FALSE,
                            write.summary = TRUE,
                            write.txtDisambig = FALSE, # passed to txtDisambig()
                            update.master = FALSE) # pulls docSetCheck.csv and updates

    # stash in .df
    results.df[k, ] <- result.v
  }
  # after last interation, write out

  write.csv(results.df, file = "results/docSetCheck.csv")
  cat("Results written to results/docSetCheck.csv\n")

}


