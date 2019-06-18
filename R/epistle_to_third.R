#' epistle_to_third()
#'
#' This function is used by txt_to_df() to transform first-person pronouns in
#' texts with first-person narrative voice.
#'
#' @param input.text A character vector of a single epistle, with punctuation left in.  Each element may be a line, sentence, paragraph, etc...
#' @param author.name A character string of the alternate name token to signal the first-person author of an epistle.  This function transforms non-dialogic 1st-person pronouns into this variable's value.  It must therefore match a unique alternate name on the associated Char.csv file. 
#' @param addressee.name A character string of the alternate name token to signal the second-person recipient of an epistle.  This function transforms 2nd-person pronouns into this variable's value.  It must therefore match a unique alternate name on the associated Char.csv file. 
#' @keywords NovNet Utilities
#'
#' @export

epistle_to_third <- function(input.text, 
                             author.name,
                             addressee.name){
  # NOTE: input.text is a text.s-type object
  # NOTE: egoname must be equivalent to a Name or Name Alternate in Char.csv
  # find all text between start quotes and end quotes
  # start quote: (//" followed by any character)
  # end quotes (//" preceded by any non-space)
  quote.v <- unlist(regmatches(input.text, gregexpr("\"\\S.*?\"\\s", input.text)))
  nonquote.v <- unlist(strsplit(input.text, "\"[^ -].*?\"[ -]"))
  nonquote.isub.v <- gsub("\\<I\\>|\\<me\\>", author.name, nonquote.v)
  nonquote.isub.v <- gsub("\\<you\\>|\\<thou\\>|\\<thee\\>", addressee.name, nonquote.v)
  
  ## stitch quote.v and nonquote.v back together.
  ## Note: text will always start with nonquote.v
  text.out.s <- paste(nonquote.isub.v, c(quote.v, ""), collapse = "")
  text.out.s
}