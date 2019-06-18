#' complex_to_third()
#'
#' This function is used by txt_to_df() to transform first-person pronouns in
#' texts with complex narrative voices.
#'
#' @param input.text A character vector of a text, with punctuation left in.  Each element may be a line, sentence, paragraph, etc...
#' @param narrator.breaks A data frame with three columns: start.chap, end.chap, egoname.  It translates 1st-person pronouns in each section (start.chap to end.chap) to the associated egoname.  Each egoname must be specified in the associated Char.csv file.
#' @keywords NovNet Utilities
#'
#'
#' @export

complex_to_third <- function(input.text, narrator.breaks){

  
  # locate chapter breaks
  chapter.pos.raw.v <- grep("^CHAPTER ", input.text)
  chapter.pos.v <- c(chapter.pos.raw.v, length(input.text) +1)
  
  text.by.narrator <- list() 
  for(i in 1:nrow(narrator.breaks)){
    temp.start.v <- chapter.pos.v[narrator.breaks[i, 1]]
    temp.end.v <- chapter.pos.v[narrator.breaks[i, 2]+1]-1
    temp.narrator <- narrator.breaks[i, 3]
    temp.text.v <- input.text[temp.start.v:temp.end.v]
    temp.text.s <- paste(temp.text.v, collapse = " ")
    text.by.narrator[[i]]  <- first_to_third(input.text = temp.text.s, egoname = temp.narrator)
  }
  text.by.narrator
}