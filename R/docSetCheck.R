#' docSetCheck()
#'
#' This function accepts a filename and:
#' a) Checks for a complete document set.
#' b) Checks for orphaned names and duplicates.
#' c) Checks for a few other problems (like badly formed metadata categories)
#' d) Returns a summary as a list
#'
#' @param filename Character string, i.e. "Crusoe"
#' @param return.result Logical.  If TRUE, returns a list summary of results.
#' @param local If FALSE (default), downloads from Google Drive and saves to folder.
#' @param write.summary Logical.  If TRUE, writes summary in filename/filename-docSetCheck.txt.
#' @param write.txtDisambig Logical. Passed to txtDisambig(). If TRUE, writes detailed report at filename/filename-txtDisambig.txt.
#' @param update.master Logical.  If TRUE looks for Results/docSetCheck.csv, and updates the relevant line.
#' @keywords NovNet Utilities
#'
#' @export


#### Document Set Check
#### This script contains docSetCheck() which accepts a filename and:
#### a) Checks for a complete document set.
#### b) Checks for orphaned names and duplicates.
#### c) Returns a report
####
#### It also contains the script to iterate over all current filenames
####



### 1) docSetCheck() ----

docSetCheck <- function(filename,
                        return.result = FALSE,
                        local = FALSE,
                        write.summary = TRUE,
                        write.txtDisambig = TRUE, # passed to txtDisambig()
                        update.master = TRUE){ # pulls docSetCheck.csv and updates
  ## a) check for folder, and create if doesn't exist
  dir.path <- paste0("data/", filename)
  if(!dir.exists(dir.path)){
    dir.create(dir.path)
  }

  ## aa) if(local == FALSE) download .csv and .txt
  if(local == FALSE){
    cat("Downloading character data from Google Drive.\n")
    drive_download(file = paste0(filename, "Char.csv"),
                   overwrite = TRUE,
                   type = "csv",
                   path = paste0("data/", filename, "/", filename, "Char.csv"))
    cat("Downloading .txt file from Google Drive.\n")
    drive_download(file = paste0(filename, ".txt"),
                   overwrite = TRUE,
                   path = paste0("data/", filename, "/", filename, ".txt"))
  }
  # Generate document metadata
  # pull preparer name from .csv file
  file.meta.v <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                          header = FALSE, sep = ",",
                          stringsAsFactors = FALSE,
                          blank.lines.skip = TRUE)[1:6, 2]
  names(file.meta.v) <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                                 header = FALSE, sep = ",",
                                 stringsAsFactors = FALSE,
                                 blank.lines.skip = TRUE)[1:6, 1]

  ## b) Check for complete document set from drive_find() results
  if(!exists("drive.files.df")){
    drive.files.df <- drive_find()
  }

  drive.files.v <- grep(filename, drive.files.df$name, value = TRUE)
  doc.set.l <- list()
  doc.set.v <- c(".txt", "Char.csv", "Chap.csv", "Report.txt", "Epistle.csv")
  for(i in 1:length(doc.set.v)){
    file.temp <- doc.set.v[i]
    regex.temp <- paste0(filename, doc.set.v[i])
    doc.set.l[[file.temp]] <- if(length(grep(regex.temp, drive.files.v)) == 1){
      "found"
    } else {
      "NOT FOUND"
    }
  }

  # fix issue of Chap.csv "X" if text has no chapters
  # check for chapters
  input.text.v <- scan(file = paste0("data/", filename, "/", filename, ".txt"),
                       what="character", sep="\n")
  num.chaps.v <- length(grep("^CHAPTER", input.text.v))
  if(num.chaps.v <= 1){
    doc.set.l[[which(doc.set.v == "Chap.csv")]] <- "n/a"
  }

  # fix issue of checking for Epistle.csv
  voice.v <- tolower(file.meta.v[4])
  epistolary.tf <- grep("(pistle|pistolary)", voice.v)
  if(length(epistolary.tf) >= 1){
    doc.set.l <- doc.set.l
  } else {
    doc.set.l$Epistle.csv <- "n/a"
  }

  ## c) Check for orphaned names and ambiguous names (run txtDisambig(write.report = FALSE))
  # check first for duplicate uniqnames

  txtDisambig.l <- txtDisambig(filename,
                               local = TRUE,
                               write.report = write.txtDisambig,
                               return.results = TRUE)

  ## d) check for metadata categories
  # pull .csv
  char.data.df <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                           header = FALSE, sep = ",",
                           skip = 0,
                           stringsAsFactors = FALSE,
                           blank.lines.skip = TRUE)
  char.data.df[is.na(char.data.df)] <- "" # removes NA from completely blank columns
  charname.col.v <- which(char.data.df[10, ] == "CharName")
  char.names.df <- char.data.df[11:nrow(char.data.df),
                                charname.col.v:ncol(char.data.df)]
  row.names(char.names.df) <- char.data.df[11:nrow(char.data.df), 1]

  # check to see if metadata has been entered
  metadata.l <- list()
  cats.l <- list()
  for(i in 1:3){
    metadata.v <- char.data.df[11:nrow(char.data.df), i+1]
    metadata.cat <- char.data.df[10, i+1]
    metadata.l[[i]] <- metadata.v
    cats.l[[i]] <- metadata.cat
  }

  cols.match.v <- sapply(metadata.l, function(x){length(x) >= 1}) ==
    sapply(cats.l, function(x){length(x) == 1})
  problem.cols <- which(cols.match.v == FALSE)

  ## === Prepare output ===
  ## y) Compile Report
  doc.sets.v <- c()
  for(i in 1:length(doc.set.l)){
    doc.sets.v[i] <- paste0("  ", filename, names(doc.set.l)[i], ": ", doc.set.l[[i]])
  }

  # pull ambiguous alt (belonging to more than one uniqname) from txtDisambig.l
  ambiguous.alt.v <- txtDisambig.l[[1]]
  if(length(ambiguous.alt.v) == 0){
    aa.for.report <- "  [[ no names need to be disambiguated ]]"
    ambiguous.alt <- "OK"
  } else {
    ambiguous.alt <- "X"
    aa.for.report <- ambiguous.alt.v
  }
  #search for orphaned alt (alt not found in text)
  alt.not.found.v <- txtDisambig.l[[2]]
  alt.not.found.v <- alt.not.found.v[-grep("^TextEgo", alt.not.found.v)] # remove TextEgo

  if(length(alt.not.found.v) == 0){
    anf.for.report <- "  [[ No alternate names are orphaned ]]"
    alt.not.found <- "OK"
  } else {
    anf.for.report <- alt.not.found.v
    alt.not.found <- "X"
  }
  if(length(problem.cols) == 0){
    pc.for.report <- "  [[ No metadata naming problems detected ]]"
    problem.cols <- "OK" # for returned results
  } else {
    pc.for.report <- c(paste("Meta", problem.cols, "  "))
    problem.cols <- "X"
  }

  out.v <- c(paste0("DOCUMENT SET REPORT for ", file.meta.v[2]),
             "",
             paste("Source txt file:    ", filename, ".txt", sep = ""),
             paste("Source csv file:    ", filename, "Char.csv", sep = ""),
             paste("Source Compiled by: ", file.meta.v[5], sep = ""),
             paste("Report Date:        ", date(), sep = ""),
             "",
             "I. DOCUMENT SET",
             "",
             print(doc.sets.v),
             "",
             "",
             "II. CHARACTER DISAMBIGUATION",
             "",
             "  Ambiguous alternate names (please disambiguate):",
             "",
             paste("  ", aa.for.report),
             "",
             "  Orphaned alternate names (please correct):",
             "",
             paste("  ", anf.for.report),
             "",
             "",
             "III. CHARACTER METADATA CATEGORIES:",
             "",
             "  Please ensure that the following metadata categories are correctly named:",
             "",
             pc.for.report,
             "",
             "",
             "END REPORT",
             "",
             "")


  ## x) save as a .txt file
  if(write.summary == TRUE){
    write(out.v, file = paste0("data/", filename, "/", filename, "-docSetCheck.txt")) # save file
    cat("File saved as", paste0(getwd(), "/data/", filename, "/", filename, "-docSetCheck.txt"), "\n")
  }

  ## y & z) generate vector summary for output
  # add filename and compiler name to temp.v
  temp.v <- c()
  temp.v[1:2] <- c(filename, file.meta.v["Compiler Name:"])

  # pull and transform Document Set, stash in temp.v
  # transform doc.sets.v "NOT FOUND|found" into "X|OK"
  doc.set.v <- unlist(doc.set.l)
  doc.set.v[which(doc.set.v == "NOT FOUND")] <- "X"
  doc.set.v[which(doc.set.v == "found")] <- "OK"
  temp.v[3:7] <- doc.set.v

  # pull and stash ambiguous.alt, alt.not.found, problem.meta
  temp.v[8:10] <- c(ambiguous.alt, alt.not.found, problem.cols)

  ## y) if(update.master = TRUE) update docSetCheck.csv
  if(update.master == TRUE){
    if(file.exists("results/docSetCheck.csv")){
      # read.csv docSetCheck.csv
      read.csv.df <- read.csv("results/docSetCheck.csv", stringsAsFactors = FALSE)
      docSetCheck.df <- read.csv.df[, 2:ncol(read.csv.df)]
      # check to see if novel already exists in docSetCheck.df
      if(filename %in% docSetCheck.df[,1]){
        docSetCheck.df[which(docSetCheck.df[,1] == filename), ] <- temp.v
      } else {
        docSetCheck.df <- rbind.data.frame(docSetCheck.df, temp.v)
        docSetCheck.df <- docSetCheck.df[order(docSetCheck.df[, 1]), ] # realphabetize
      }
      # write out
      write.csv(docSetCheck.df, file = "results/docSetCheck.csv")

    } else {
      cat("file docSetCheck.csv not found; run docSetComplete() to compile.\n")
    }
  }

  ## z) if(return.result == TRUE) then return vector summary of results
  return.result.v <- temp.v
    if(return.result == TRUE){
    return(temp.v)
  }
}




