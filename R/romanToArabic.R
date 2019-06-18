#### Roman numeral conversion romanToArabic()

### 1) Roman Numeral Conversion ----
## Transforms Roman Numerals in "^CHAPTER [[:upper:]]+$" to arabic--
## Works for .txt files generated from Project Gutenberg

#' Roman Numeral Conversion
#'
#' This function converts roman numeral chapter numbers to arabic, from Project Gutenberg files.
#' It expects chapter headings in this format: CHAPTER I, CHAPTER II, etc...
#'
#' @param filename File name as character string, i.e. "Crusoe".
#'
#' @keywords Disambiguation
#'
#'
#' @export

romanToArabic <- function(filename){
  {file.v <- paste0("data/", filename, "/", filename, ".txt")
  input.text.v <- scan(file = file.v,
                       what="character",
                       sep="\n",
                       blank.lines.skip = FALSE)
  rm(file.v)}
  chapter.lines.v <- grep("^(CHAPTER [[:upper:]]+)|(CHAPTER [[:upper:]]+\\.)$", input.text.v) # pulls CHAPTER lines
  roman.numbers.v <- gsub("^CHAPTER ([[:upper:]]+).*", "\\1", input.text.v[chapter.lines.v])
  arabic.numbers.v <- as.numeric(as.roman(roman.numbers.v)) # transform roman to arabic
  input.text.v[chapter.lines.v] <- paste("CHAPTER", arabic.numbers.v, sep = " ")
  cat("Found and transformed", as.character(as.roman(length(roman.numbers.v))), "roman chapter heads to arabic equivalents.\n")
  write(input.text.v, file = paste0("data/", filename, "/", filename, "-arabic.txt")) # overwrite file
  cat("File saved as", paste0(getwd(), "data/", filename, "/", filename, "-arabic.txt", "\n"))
}




