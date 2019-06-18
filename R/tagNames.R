#' tagNames()
#' tagNames() produces a .txt file with all found alternate names
#'   tagged with NovNet-style tags:
#'   < Alternate Name u: uniqname \>
#'
#' @param filename Character.  Filename as single character string, i.e. "Crusoe".
#' @param local If FALSE (default), downloads from Google Drive and saves to folder.
#' @param include.all Logical vector or numerical range. If TRUE (default), processes whole text.  If range (i.e. 1:18), processes only those chapters.  If FALSE, will offer a prompt to determine a range.
#'
#'
#' @import googledrive
#' @import tm
#' @import stringi
#'
#' @keywords NovNet Utilities PhaseII
#'
#' @import googledrive
#'
#' @export



### 1) tagNames() ----


tagNames <- function(filename,
                     local = FALSE,
                     include.all = TRUE){
  ## NOTE: prep by running linesToParas()
  ## a)
  # pull .txt file
  file.v <- paste0(filename, ".txt")
  if(!dir.exists(paste0("data/", filename))){
    dir.create(paste0("data/", filename))
  }
  file.path.v <- paste0("data/", filename, "/", file.v)
  if(local == FALSE){
    cat("Downloading .txt file from Google Drive.\n")
    drive_download(file.v, overwrite = TRUE, path = file.path.v)
  }
  input.text.v <- scan(file = file.path.v,
                       what="character",
                       sep="\n",
                       blank.lines.skip = FALSE)

  ## b) locates the body of the text (and stores the paratext)
  start.v <- which(input.text.v == "====START====")
  end.v <- which(input.text.v == "====END====")
  frontmatter.v <- input.text.v[1:start.v]
  backmatter.v <- input.text.v[end.v:(length(input.text.v))]
  main.text.v <- input.text.v[(start.v+1):(end.v-1)]

  ## c) does some very light (reversible) text prep -- including storing paragraph breaks
  text.s <- paste(main.text.v, collapse = "\n") # collapses to a single string, stores para breaks

  ## d) pull *Char.csv spreadsheet of name alternates
  if(local == FALSE){
    cat("Downloading character data from Google Drive.\n")
    drive_download(file = paste0(filename, "Char.csv"),
                   overwrite = TRUE,
                   type = "csv",
                   path = paste0("data/", filename, "/", filename, "Char.csv"))
  }
  ### Determine .csv Version ##
  char.data.version <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                                header = FALSE, sep = ",",
                                skip = 0,
                                stringsAsFactors = FALSE,
                                blank.lines.skip = FALSE)[7, 2]
  if(char.data.version != "2"){
    char.data.df <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                             header = TRUE, sep = ",",
                             skip = 7, row.names = 1,
                             stringsAsFactors = FALSE,
                             blank.lines.skip = TRUE)
    char.data.df[is.na(char.data.df)] <- "" # removes NA from completely blank columns
    if(ncol(char.data.df) > 9){
      char.names.df <- char.data.df[, c(1:6, 10:ncol(char.data.df))]
    } else {
      char.names.df <- char.data.df[, 1:6]
    }
  }
  if(char.data.version == "2"){
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
  }


  ## e) create name.alt.df data frame, with columns of:
  ##     i) name alternates
  ##     ii) uniqnames for each name alternate (rowname in char.data.df)
  ##     iii) regular expressions to locate those name alternates
  ##     iv) sticky names for ngrams (i.e. replaces " " with "_")

  # create char.names.df from char.data.df
  alt.pos.m <- which(char.names.df != "", arr.ind = TRUE) #returns matrix in format:
  #               row       column
  # uniqname
  # bind each alternate name (recorded as [r,c] position in alt.pos.v) to rowname
  # in new two-column matrix, so each alternate name is bound to its uniqname
  alt.uniq.pair <- cbind(char.names.df[alt.pos.m], rownames(alt.pos.m))
  #               alternatename       uniqname
  # [1,]

  # strip whitespace from names in ordered.pair.m
  alt.uniq.pair <- gsub(" $", "", alt.uniq.pair)
  alt.uniq.pair <- gsub("^ ", "", alt.uniq.pair)
  # transform to data frame
  alt.sticky.df <- as.data.frame(alt.uniq.pair, stringsAsFactors = FALSE)
  colnames(alt.sticky.df) <- c("alt", "uniqname")
  # remove accidental spaces in uniqnames
  alt.sticky.df$uniqname <- gsub(" ", "", alt.sticky.df$uniqname)
  # Transform alternate names to Regular Expressions in alt.sticky.df
  regex.raw.v <- alt.sticky.df$alt
  # solve a problem: alternate names ending in "." or "-"
  # because the regex anchor for the end of a word only accepts
  # conditions where a letter is followed by a space or punctuation,
  # names like "Lord A." won't be found.
  regex.v <- paste("\\<", # signals beginning of word
                   regex.raw.v,
                   "(\\W)", # signals non-word character, and isolates for back-referencing
                   sep = "")
  alt.sticky.df$regex <- regex.v

  # Transform names to sticky names and bind to alt.sticky.df
  alt.sticky.df$sticky <- gsub(" ", "_", alt.sticky.df$alt)
  # Generate a vector of sticky regex, to locate $sticky
  alt.sticky.df$stickyRegex <- gsub(" ", "_", alt.sticky.df$regex)

  # Generate a vector of unique flags (distinct from uniqnames)
  alt.sticky.df$flag <- paste0(alt.sticky.df$uniqname, "Flag")

  ## f) sorts the data frame by name alternate, from longest ngram to shortest

  alt.sticky.df <- alt.sticky.df[order(sapply(strsplit(alt.uniq.pair[, 1], " "), length),
                                       decreasing = TRUE), ]
  rownames(alt.sticky.df) <- 1:nrow(alt.sticky.df)


  ## g) uses gsub() embedded in a for() loop to replace all the name alternates with sticky alternates

  # create progress bar
  hash.length.v <- length(alt.sticky.df$regex)/10
  prog.hash.v <- round(seq(from = 0,
                           to = length(alt.sticky.df$regex),
                           by = hash.length.v), digits = 0)
  prog.hash.ticker <- 2
  alt.not.found <- NULL
  cat("Finding and tagging name alternates.
      This may take a while.")
  if(nrow(alt.sticky.df) > 0){
    for(i in 1:nrow(alt.sticky.df)){
      #if(length(grep(alt.sticky.df$regex[i], text.s)) == 0){
      #  alt.not.found <- c(alt.not.found, subset.df$alt[i])
      #}
      if(i == prog.hash.v[prog.hash.ticker]){
        prog.hash.ticker <- prog.hash.ticker + 1
        cat("..")
      }
      flagged.name.v <- paste0(alt.sticky.df$sticky[i], " ",
                               alt.sticky.df$flag[i],
                               "\\1") # backreferences (\\W) from end of alt.sticky.df$regex[i]
      text.s <- gsub(alt.sticky.df$regex[i], flagged.name.v, text.s)
    }
  }
  cat("\n")
  ## i) reverses step g), replacing sticky names with names,
  ##                      replacing flags with uniqnames
  ##                      and creating a correctly formed tag by adding < u: \>
  # reverse order of alt.sticky.df, to find shortest sticky-names first
  alt.sticky.df <- alt.sticky.df[nrow(alt.sticky.df):1, ]

  # create hash ticker
  prog.hash.v <- round(seq(from = 0,
                           to = length(alt.sticky.df$regex),
                           by = hash.length.v), digits = 0)
  prog.hash.ticker <- 2

  cat("Removing sticky names and forming front half of character tags.
      Please be patient.")
  if(nrow(alt.sticky.df) > 0){
    for(i in 1:length(alt.sticky.df$regex)){
      if(i == prog.hash.v[prog.hash.ticker]){
        prog.hash.ticker <- prog.hash.ticker + 1
        cat("..")
      }
      text.s <- gsub(alt.sticky.df$stickyRegex[i],
                     paste0("< ", # angle-brace signals beginning of character tag
                            alt.sticky.df$alt[i],
                            "\\1"), # backreferences \\W from stickyRegex
                     text.s)
    }
  }
  cat("\n")

  cat("Adding uniqnames and forming back half of character tags.
      Almost finished.")
  prog.hash.v <- round(seq(from = 0,
                           to = length(alt.sticky.df$regex),
                           by = hash.length.v), digits = 0)
  prog.hash.ticker <- 2

  if(nrow(alt.sticky.df) > 0){
    for(i in 1:length(alt.sticky.df$regex)){
      if(i == prog.hash.v[prog.hash.ticker]){
        prog.hash.ticker <- prog.hash.ticker + 1
        cat("..")
      }

      text.s <- gsub(alt.sticky.df$flag[i],
                     paste0("u: ",
                            alt.sticky.df$uniqname[i],
                            " />"),
                     text.s)
    }
  }
  cat("\n")

  ## h) glues the text file back together
  #text.s <- gsub("\n", "\n\n", text.s)
  main.out.v <- unlist(strsplit(text.s, "\n"))
  #main.out.v <- gsub("^ | $", "", main.out.v)
  text.out.v <- c(frontmatter.v, main.out.v, backmatter.v)

  # pull preparer name from .csv file
  preparer.v <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                         header = FALSE, sep = ",",
                         stringsAsFactors = FALSE,
                         blank.lines.skip = TRUE)[5, 2]

  cat("Generating header.\n")
  out.header.v <- c("BEGIN HEADER:",
                    "",
                    paste("Source txt file:    ", filename, ".txt", sep = ""),
                    paste("Source csv file:    ", filename, "Char.csv", sep = ""),
                    paste("Source Compiled by: ", preparer.v, sep = ""),
                    paste("This text date:     ", date(), sep = ""),
                    "",
                    "This text compiled by function tagNames(), part of the
                    novel-networks package for R.",
                    "",
                    "NOTE: tagNames() will not flag names that end in
                    punctuation marks, such as Lord A. or Mrs. B---",
                    "",
                    "BEGIN TEXT:",
                    ""
  )

  out.v <- c(out.header.v, text.out.v)

  ## j) saves as a .txt file

  write(out.v, file = paste0("data/", filename, "/", filename, "-tagNames.txt")) # save file
  cat("File saved as", paste0(getwd(), "/data/", filename, "/", filename, "-tagNames.txt", "\n"))
}

# tagNames()




