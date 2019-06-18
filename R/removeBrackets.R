#' removeBrackets()
#'
#' 2) removeBrackets() Remove bracketed names ----
#' this is a companion function to txtDisambig().  It:
#' a) searches for stray bracketed names.
#' b) strips out stray brackets.
#' c) locates the errata section
#'    i) removes the errata
#'    iii) saves as .txt file
#'
#' @param filename File name as character string, i.e. "Crusoe".
#' @param local Default = TRUE. If FALSE, searches for file in google drive. If TRUE, seeks file in folder filename/.
#'
#' @keywords Disambiguation
#'
#' @import googledrive
#'
#' @export

removeBrackets <- function(filename,
                           local = TRUE){
  ## a)
  # pull .txt file

  file.v <- paste0(filename, "-textprep.txt")
  if(!dir.exists(paste0("data/", filename))){
    dir.create(paste0("data/", filename))
  }
  file.path.v <- paste0("data/", filename, "/", file.v)
  if(local == FALSE){
    cat("Downloading .txt file from Google Drive.\n")
    drive_download(file.v, overwrite = TRUE, path = file.path.v)
  }

  {file.v <- paste0("data/", filename, "/", filename, "-textprep.txt")
    input.text.v <- scan(file = file.v,
                         what="character",
                         sep="\n",
                         blank.lines.skip = FALSE)
    rm(file.v)}

  ## b) locates the body of the text (and stores the paratext)
  start.v <- which(input.text.v == "====START====")
  end.v <- which(input.text.v == "====END====")
  frontmatter.errata.v <- input.text.v[1:start.v]
  backmatter.v <- input.text.v[end.v:(length(input.text.v))]
  main.text.v <- input.text.v[(start.v+1):(end.v-1)]

  ## c) locates the ERRATA
  errata.begin <- which(frontmatter.errata.v == "BEGIN ERRATA:")
  errata.end <- which(frontmatter.errata.v == "END ERRATA")
  errata.v <- frontmatter.errata.v[errata.begin:errata.end]
  frontmatter.v <- frontmatter.errata.v[(errata.end+1):length(frontmatter.errata.v)]

  ## c) does some very light (reversible) text prep -- including storing paragraph breaks
  text.s <- paste(main.text.v, collapse = "\n") # collapses to a single string

  ## d) finds stray brackets and removes
  text.s <- gsub("\\[ | \\]", "", text.s)

  ## h) glues the text file back together

  main.out.v <- unlist(strsplit(text.s, "\n"))
  main.out.v <- gsub("^ | $", "", main.out.v)
  text.out.v <- c(frontmatter.v, main.out.v, backmatter.v)

  ## j) saves as a .txt file

  write(text.out.v, file = paste0("data/", filename, "/", filename, "-removeBrackets.txt")) # save file
  cat("File saved as", paste0(getwd(), "data/", filename, "/", filename, "-removeBrackets.txt", "\n"))
}
