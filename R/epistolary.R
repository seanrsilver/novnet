#' epistolary()
#' This function is used by Txt_to_df() to transform epistolary authors and addressees into chracter names.
#' 1) Splits input text.v into epistles
#' 2) Applies first_to_third() selectively, to transform first and second
#'    person address in epistles into sender and addressee uniqnames
#'
#' REQUIRES: a) *Epistle.csv, a list of letters with sender and addressee
#'              Filename must match .txt file
#'           b) Letters headers in .txt on their own line, at the start of the line, in one of two formats:
#'              EPISTLE [number]
#'              LETTER [number]
#'
#' @param input.text A text transformed into a character vector; each element is a line of text. Txt_to_df() returns a correctly formatted vector as $main.text.v.
#' @keywords NovNet Utilities
#'
#'
#' @export



epistolary <- function(input.text = main.text.v){

  # split text into epistles
  epistle.pos.v <- grep("(^EPISTLE [0-9.]+)|(^LETTER [0-9.]+)", input.text)
  if(epistle.pos.v[1] != 1){
    prefatory.paratext.v <- input.text[1:epistle.pos.v[1]]
  }
  epistles.l <- list()
  for(i in 1:length(epistle.pos.v)){
    start <- epistle.pos.v[i]
    if(i == length(epistle.pos.v)){
      end <- length(input.text)
    } else {
      end <- epistle.pos.v[i+1]-1
    }
    epistles.l[[i]] <- input.text[start:end]
  }

  # pull epistle data from *Epistle.csv file
  epistle.csv.df <- read.csv(file = paste0("data/", filename, "/", filename, "Epistle.csv"),
                              header = FALSE, sep = ",",
                              skip = 0,
                              stringsAsFactors = FALSE)
  epistle.data.df <- epistle.csv.df[11:nrow(epistle.csv.df), 1:3]
  colnames(epistle.data.df) <- epistle.csv.df[10, 1:3]

  # run first_to_third on each element of the list
  # to transform I/me into epistle.data.df$from
  source(file = paste0(getwd(), "/code/first_to_third.R"), local = FALSE)
  epistles.epistle_to_third.l <- list()

  for(j in 1:nrow(epistle.data.df)){
    epistle.s <- paste(epistles.l[[j]], collapse = " ")
    author.name.v <- epistle.data.df$from[j]
    addressee.name.v <- epistle.data.df$to[j]
    epistle.epistle_to_third.s <- epistle_to_third(input.text = epistle.s,
                                                 author.name = author.name.v,
                                                 addressee.name = addressee.name.v)
    epistles.epistle_to_third.l[[j]] <- epistle.epistle_to_third.s
  }

  # paste everything back together
  output.s <- do.call(paste, epistles.epistle_to_third.l)
  output.s
}
