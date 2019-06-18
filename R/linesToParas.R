#### Paragraph Standardization, linesToParas()

### linesToParas() ----
### Standardize from Gutenberg linebreaks to 1 paragraph per line, with blank line in-between

#' Lines to Paragraphs
#'
#' This function accepts a .txt file with Project Gutenberg format
#' and removes inner-paragraph line breaks.
#'
#' @param filename File name as character string, i.e. "Crusoe", or (if scan.file = FALSE) a character vector of a text where each string is a line of text.
#' @param write.out Logical.  If TRUE (default), writes to file filename/filename-linesToParas.txt.
#' @param return.text Logical.  If TRUE (default = FALSE), returns the transformed text as a vector.
#' @param scan.file Logical.  If TRUE (default), scans in filename from filename/filename.txt, or (if local = FALSE) Google Drive filename.txt.
#' @param local If FALSE (default), downloads from Google Drive and saves to folder filename/.
#' @keywords Text Preparation
#'
#' @import googledrive
#'
#' @export
# scan into R environment


linesToParas <- function(filename, # name of file to scan, or of input.text.v if scan.file = FALSE
                         write.out = TRUE,
                         return.text = FALSE,
                         scan.file = TRUE,
                         local = TRUE){
  ## a)
  # pull .txt file
  if(scan.file == TRUE){
    file.v <- paste0("data/", filename, "/", filename, ".txt")
    if(!dir.exists(paste0("data/", filename))){
      dir.create(paste0("data/", filename))
    }
    if(local == FALSE){
      cat("Downloading .txt file from Google Drive.\n")
      drive_download(file = paste0(filename, ".txt"),
                     overwrite = TRUE,
                     type = "txt",
                     path = paste0("data/", filename, "/", filename, ".txt"))
    } else {
      cat("Loading .txt file locally from:", paste0(getwd(), "/data/", filename, ".txt\n"))
    }
    input.text.v <- scan(file = file.v,
                         what="character",
                         sep="\n",
                         blank.lines.skip = FALSE)
  } else { # i.e. if scan.file != TRUE
    input.text.v <- filename
  }

  ## b) locates the body of the text (and stores the paratext)
  start.v <- which(input.text.v == "====START====")
  end.v <- which(input.text.v == "====END====")
  frontmatter.v <- input.text.v[1:start.v]
  backmatter.v <- input.text.v[end.v:(length(input.text.v))]
  main.text.v <- input.text.v[(start.v+1):(end.v-1)]
  # store length for out message
  number.lines.v <- length(main.text.v)

  ## c) store blank lines as double paragraph breaks, and transform to single string
  main.text.breaks.v <- gsub("^$", "PARAGRAPHBREAK", main.text.v)
  main.text.breaktabs.v <- gsub("^    ", "INDENTTAB    ", main.text.breaks.v)
  text.breaks.s <- paste(main.text.breaktabs.v, collapse = " ") # collapses to a single string

  ## breaks the text back into lines, this time by paragraphs with blank line separation
  text.breaks.v <- unlist(strsplit(text.breaks.s, "PARAGRAPHBREAK "))
  # NOTE: this leaves a final "PARAGRAPHBREAK" at the end, which must be removed
  if(text.breaks.v[length(text.breaks.v)] == "PARAGRAPHBREAK"){
    text.breaks.v <- text.breaks.v[1:(length(text.breaks.v)-1)]
  }

  # store a blank element between each paragraph-- so that linesToParas could
  # potentially be run again
  text.breaks.m <- rbind(text.breaks.v, "BLANK")
  text.paras.v <- as.character(text.breaks.m)
  # split poetry and other indented text
  text.paras.l <- strsplit(text.paras.v, "INDENTTAB")
  text.paras.out <- unlist(text.paras.l)

  # eliminate blank lines
  text.paras.out <- text.paras.out[which(text.paras.out != "")]
  # transform "BLANK" into ""
  text.paras.out <- gsub("^BLANK$", "", text.paras.out)

  number.paras.out <- length(text.paras.out) # store length of output
  ## h) glues the text file back together

  text.out.v <- c(frontmatter.v, "", text.paras.out, backmatter.v)

  ## j) saves as a .txt file
  if(write.out == TRUE){
    write(text.out.v, file = paste0("data/", filename, "/", filename, "-linesToParas.txt")) # save file
    cat(number.lines.v, "input lines transformed into", number.paras.out, "output paragraphs.\n",
        "File saved as", paste0(getwd(), "/data/", filename, "/", filename, "-linestoParas.txt", "\n"))
  }
  if(return.text == TRUE){
    text.paras.out
  }
}

#linesToParas(filename = filename.v, local = TRUE)



