#' autoReplaceList()
#'
#' This function is a wrapper for autoReplace.  It searches a NovNet .txt file
#' for a correctly formatted regular expression and substitutes a replacement.
#'
#' It is useful for deleting page numbers, formatting chapter headings,
#' cleaning embedded notes, and so on.
#'
#' @param filename File name as character string, i.e. "Crusoe".
#' @param patterns List of length-2 vectors, format c("pattern", "replace"). Expects correctly formatted regular expressions.
#' @param local Default = TRUE. If FALSE, searches for file in google drive. If TRUE, seeks file in folder filename/.
#' @keywords Text Preparation
#'
#' @import googledrive
#'
#' @export

autoReplaceList <- function(filename = filename.v,
                         patterns = patterns.l, # a list of length-2 vectors, format c("pattern", "replace")
                         local = TRUE){
  ## a)
  # pull .txt file
  file.v <- paste0("data/", filename, "/", filename, ".txt")
  if(!dir.exists(paste0("data/", filename))){
    dir.create(paste0("data/", filename))
  }
  if(local == FALSE){
    cat("Downloading character data from Google Drive.\n")
    drive_download(file = paste0(filename, ".txt"),
                   overwrite = TRUE,
                   type = "txt",
                   path = paste0("data/", filename, "/", filename, ".txt"))
  } else {
    cat("Loading file locally from:", paste0(getwd(), "/data/", filename, ".txt\n"))
  }
  input.text.v <- scan(file = file.v,
                       what="character",
                       sep="\n",
                       blank.lines.skip = FALSE)

  ## b) locates the body of the text (and stores the paratext)
  start.v <- which(input.text.v == "====START====")
  end.v <- which(input.text.v == "====END====")
  frontmatter.v <- input.text.v[1:start.v]
  backmatter.v <- input.text.v[end.v:(length(input.text.v))]
  main.text.v <- input.text.v[(start.v+1):(end.v-1)]

  ## c)  and d) uses for() loop to iterate through main.text.v and replace
  num.matches.l <- list()
  main.text.out.v <- main.text.v
  alterations.v <- NULL

  for(i in 1:length(patterns)){
    ## c) finds number of instances of pattern
    pattern.matches.l <- regmatches(main.text.out.v, gregexpr(patterns[[i]][1], main.text.v))
    pattern.matches.v <- do.call(c, pattern.matches.l)
    num.matches.v <- length(pattern.matches.v)

    ## d) finds patterns and deletes (replaces with "")
    main.text.out.v <- gsub(patterns[[i]][1], patterns[[i]][2], main.text.out.v)

    alterations.v[i] <- paste0(num.matches.v, " matches of regex '", patterns[[i]][1], "' found and replaced with '", patterns[[i]][2], "'.")
  }

  ## e) creates a brief report and attaches to the head of the document
  report.v <- c("This file was cleaned for the Novel Networks project:",
                "",
                alterations.v,
                "",
                date(),
                "",
                "")

  ## f) pastes back together
  text.out.v <- c(report.v, frontmatter.v, main.text.out.v, backmatter.v)

  ## j) saves as a .txt file

  write(text.out.v, file = paste0("data/", filename, "/", filename, "-autoReplaceList.txt")) # save file
  cat("File saved as", paste0(getwd(), "/data/", filename, "/", filename, "-autoReplaceList.txt", "\n"))
}


