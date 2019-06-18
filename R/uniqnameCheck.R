#' uniqnameCheck()
#'
#' Checks for duplicate rownames in Char.csv uniqname, and returns a matrix
#' Run if txtDisambig fails with "duplicate row names not allowed"
#'
#' @param filename File name as character string, i.e. "Crusoe".
#' @param local Default = TRUE. If FALSE, searches for file in google drive. If TRUE, seeks file in folder filename/.
#' @keywords Text Preparation
#'
#' @import googledrive
#'
#' @export

uniqnameCheck <- function(filename,
                          local = FALSE){

  ## a) pull *Char.csv spreadsheet of name alternates
  if(local == FALSE){
    cat("Downloading character data from Google Drive.\n")
    drive_download(file = paste0(filename, "Char.csv"),
                   overwrite = TRUE,
                   type = "csv",
                   path = paste0("data/", filename, "/", filename, "Char.csv"))
  }

  char.data.df <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                           header = TRUE, sep = ",",
                           skip = 7,
                           stringsAsFactors = FALSE,
                           blank.lines.skip = TRUE)
  char.data.df[is.na(char.data.df)] <- "" # removes NA from completely blank columns

  ## Pull vector of uniqnames-- char.data.df[, 1]
  uniq.names.v <- char.data.df[, 1]
  uniq.names.t <- table(uniq.names.v)
  dup.uniqnames.t <- uniq.names.t[which(uniq.names.t > 1)]

  ## Return to screen as matrix, or report no duplicates found
  dup.uniqnames.df <- data.frame(uniqname = names(dup.uniqnames.t), numOfDups = as.numeric(dup.uniqnames.t))
  if(nrow(dup.uniqnames.df) == 0){
    cat("No duplicate uniqnames have been found.")
  } else {
    dup.uniqnames.df
  }
}
