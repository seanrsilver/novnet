#### autoReplace(filename, x), automatically finds and replaces text w/ gsub
####    from *.txt file (and saves as *-autoRemove.txt)

### 3) autoReplace() ----

#' Auto Replace
#'
#' This function searches a NovNet .txt file for a correctly formatted regular expression
#' and substitutes a replacement.
#'
#' It is useful for deleting page numbers, formatting chapter headings,
#' cleaning embedded notes, and so on.
#'
#' @param filename File name as character string, i.e. "Crusoe".
#' @param pattern Regular expression of text to be replaced.
#' @param replacemt Regular expression replacement.  Accepts all correctly formatted regular expressions.
#' @param local Default = TRUE. If FALSE, searches for file in google drive. If TRUE, seeks file in folder filename/.
#' @keywords Text Preparation
#'
#' @import googledrive
#'
#' @export

autoReplace <- function(filename,
                       pattern,
                       replacemt = "",
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

  ## c) finds number of instances of pattern
  pattern.matches.l <- regmatches(main.text.v, gregexpr(pattern, main.text.v))
  pattern.matches.v <- do.call(c, pattern.matches.l)
  num.matches.v <- length(pattern.matches.v)

  ## d) finds patterns and deletes (replaces with "")
  main.text.out.v <- gsub(pattern, replacemt, main.text.v)

  ## e) pastes back together
  text.out.v <- c(frontmatter.v, main.text.out.v, backmatter.v)

  ## j) saves as a .txt file

  write(text.out.v, file = paste0("data/", filename, "/", filename, "-autoRemove.txt")) # save file
  cat("", num.matches.v, "matches of", pattern, "found and replaced with", replacemt, ".\n",
      "File saved as", paste0(getwd(), "/data/", filename, "/", filename, "-autoRemove.txt", "\n"))
}

#autoReplace(filename = filename.v, pattern = "\\[Page [0-9]*?\\]", replace = "", local = TRUE)

