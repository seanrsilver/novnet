#' functionName()
#'
#' Disambiguation Assistant ----
#' This function:
#' a) pulls a .txt file
#' b) locates the body of the text (and stores the metadata)
#' c) does some very light (reversible) text prep -- including storing paragraph breaks
#' d) pulls *Char.csv name alternate file
#' e) creates a name.alt.df data frame, with columns of:
#'     i) name alternates
#'     ii) regular expressions to locate those name alternates
#'     iii) uniqnames for each name alternate (column 1)
#'     iv) sticky names for ngrams (i.e. replaces " " with "_")
#' f) sorts the data frame by name alternate, from longest (most spaces) to shortest
#' g) uses gsub() embedded in a for() loop to replace all the name alternates with sticky alternates
#' h) glues the text file back together and saves it as a .txt file
#'
#' i) finds all duplicate name alternates, and returns a matrix collated with uniqnames
#'
#' j) NOTE: a companion function removeBrackets removes all brackets and sticky spaces.
#'
#' @param filename Character string of filename with associated .txt and Char.csv files.
#' @param local Logical vector.  If FALSE (default), looks in Google Drive for files.  If TRUE, looks for filename in a folder with path data/filename/.
#' @param write.report Logical vector.  If TRUE (default), writes a -txtDisambig file with results.
#' @param return.results Logical vector.  Default is FALSE.  If TRUE returns a list of length 2: [[1]] a vector of names associated with more than one uniqname, and [[2]] names not found in the associated text file.
#'
#' @keywords NovNet Utilities
#'
#' @import googledrive
#'
#' @export


txtDisambig <- function(filename,
                        local = FALSE,
                        write.report = TRUE,
                        return.results = FALSE){
  ## NOTE: prep by running linesToParas() in txt_prep.R
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

  ## f) sorts the data frame by name alternate, from longest ngram to shortest

  alt.sticky.df <- alt.sticky.df[order(sapply(strsplit(alt.uniq.pair[, 1], " "), length),
                                       decreasing = TRUE), ]
  rownames(alt.sticky.df) <- 1:nrow(alt.sticky.df)

  # find all the duplicate alternates in alt.sticky.df$alt
  duplicate.alt.v <- names(which(table(alt.sticky.df$alt) > 1)) # list of duplicates
  # for disambiguation
  # create .df of all alts that are not duplicates
  subset.df <- alt.sticky.df # subset.df are all names that do not need to be disambiguated
  # duplicate.alt.df <- alt.sticky.df
  if(length(duplicate.alt.v) > 0){
    for(i in 1:length(duplicate.alt.v)){
      subset.df <- subset(subset.df, alt != duplicate.alt.v[i])
    }
  }
  # create .df of all duplicate alts with uniqname, regex, sticky
  # duplicate.alt.df <- duplicate.alt.df[duplicate.alt.df$alt %in% duplicate.alt.v == TRUE, ]

  ## g) uses gsub() embedded in a for() loop to replace all the name alternates with sticky alternates

  # create progress bar
  hash.length.v <- length(subset.df$regex)/10
  prog.hash.v <- round(seq(from = 0,
                           to = length(subset.df$regex),
                           by = hash.length.v), digits = 0)
  prog.hash.ticker <- 2
  alt.not.found <- NULL
  cat("Replacing non-ambiguous name alternates with sticky names.
      This may take a while.")
  if(nrow(subset.df) > 0){
    for(i in 1:nrow(subset.df)){
      if(length(grep(subset.df$regex[i], text.s)) == 0){
        alt.not.found <- c(alt.not.found, subset.df$alt[i])
      }
      if(i == prog.hash.v[prog.hash.ticker]){
        prog.hash.ticker <- prog.hash.ticker + 1
        cat("..")
      }
      text.s <- gsub(subset.df$regex[i],
                     paste0(subset.df$sticky[i], "\\1"), # backreferences \\W from regex
                     text.s)
    }
  }

  ## h) brackets all ambiguous names: "[ alt ]"
  # use duplicate.alt.v, a vector of duplicate names (with no duplicates!)
    # to pull pairs from alt.sticky.df
  duplicate.alt.df <- data.frame(alt = character(),
                                 regex = character(),
                                 sticky = character(),
                                 stickyRegex = character())
  for(i in 1:length(duplicate.alt.v)){
    temp.duplicates.df <- alt.sticky.df[which(alt.sticky.df$alt == duplicate.alt.v[i]), ]
    temp.duplicates.df <- temp.duplicates.df[1, c("alt", "regex", "sticky", "stickyRegex")]
    duplicate.alt.df <- rbind.data.frame(duplicate.alt.df, temp.duplicates.df)
  }
  # order long to short
  duplicate.alt.df <- duplicate.alt.df[order(sapply(strsplit(duplicate.alt.df[, "alt"], " "), length),
                                       decreasing = TRUE), ]
  rownames(duplicate.alt.df) <- 1:nrow(duplicate.alt.df)
  
  cat("\nBracketing all ambiguous names.\n")
  # first bracket and replace names with sticky names
  for(i in 1:nrow(duplicate.alt.df)){
    text.s <- gsub(duplicate.alt.df$regex[i],
                   paste0("[ ", duplicate.alt.df$sticky[i], " ]\\1"),
                   text.s)
  }
  # now remove sticky names and replace with original alt
  for(i in 1:nrow(duplicate.alt.df)){
    text.s <- gsub(duplicate.alt.df$stickyRegex[i], 
                   paste0(duplicate.alt.df$alt[i], "\\1"), 
                   text.s)
  }

  ## i) reverses step g), replacing sticky names with names
  cat("Removing sticky names.")
  prog.hash.ticker <- 2 # resetting prog.hash.ticker

  if(nrow(subset.df) > 0){
    for(i in 1:length(subset.df$regex)){
      if(i == prog.hash.v[prog.hash.ticker]){ #hash ticker
        prog.hash.ticker <- prog.hash.ticker + 1
        cat("..")
      }
      text.s <- gsub(subset.df$stickyRegex[i],
                     paste0(subset.df$alt[i], "\\1"), # backreferences \\W from stickyRegex
                     text.s)
    }
  }
  cat("\n")

  ## h) glues the text file back together
  #text.s <- gsub("\n", "\n\n", text.s)
  main.out.v <- unlist(strsplit(text.s, "\n"))
  #main.out.v <- gsub("^ | $", "", main.out.v)
  text.out.v <- c(frontmatter.v, main.out.v, backmatter.v)

  ## i) pairs duplicate name alternates with uniqnames, and pastes it at the head of text.out.v
  ambiguous.alt <- NULL
  if(length(duplicate.alt.v) > 0){
    for(i in 1:length(duplicate.alt.v)){
      uniqname.v <- alt.sticky.df$uniqname[which(alt.sticky.df == duplicate.alt.v[i])]
      uniqname.v <- uniqname.v[!is.na(uniqname.v)]
      uniqname.s <- paste("'", uniqname.v, "'", collapse = ", ")
      ambiguous.alt[i] <- paste("Alternate name [ ",
                                duplicate.alt.v[i],
                                " ] refers to uniqnames: ",
                                uniqname.s,
                                sep = "")
    }
  }

  ambiguous.alt.out <- duplicate.alt.v
  if(length(ambiguous.alt) == 0){
    ambiguous.alt <- "  [[ no names need to be disambiguated ]]"
    ambiguous.alt.out <- c()
  }
  alt.not.found.out <- alt.not.found
  if(length(alt.not.found) == 0){
    alt.not.found <- "  [[ No alternate names are orphaned ]]"
    alt.not.found.out <- c()
  }

  # prepare the errata message
  # pull preparer name from .csv file
  preparer.v <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                         header = FALSE, sep = ",",
                         stringsAsFactors = FALSE,
                         blank.lines.skip = TRUE)[5, 2]

  cat("Generating errata report.\n")
  out.errata.v <- c("BEGIN ERRATA:",
                    "",
                    paste("Source txt file:    ", filename, ".txt", sep = ""),
                    paste("Source csv file:    ", filename, "Char.csv", sep = ""),
                    paste("Source Compiled by: ", preparer.v, sep = ""),
                    paste("Errata Date:        ", date(), sep = ""),
                    "",
                    "DIRECTIONS TO COMPILER:",
                    "NOTE: Please MAKE ALL CHANGES IN THIS .txt FILE and upload with the",
                    "      -textprep.txt extension.",
                    "",
                    "  DISAMBIGUATION",
                    "",
                    "  Please disambiguate the following alternate names:",
                    "",
                    paste("    ", ambiguous.alt),
                    "",
                    "  Instructions (if names require disambiguation): ",
                    "",
                    "  |  1) replace one ambiguous name in the Char.csv file with distinct",
                    "  |     alternate names--",
                    "  |     i.e. replace 'Harriet' in the Alt columns with 'Harriet1' and 'Harriet2'",
                    "  |",
                    "  |  2) use the FIND/REPLACE function in your text editor to find every",
                    "  |     instance of the ambiguous name alternate (i.e. '[ Harriet ]') and replace",
                    "  |     with the disambiguated alternates (i.e. 'Harriet1', 'Harriet2', etc...)",
                    "  |     NOTE: Ambiguous name alternates are identified in the .txt file with brackets.",
                    "  |     This is to aid searching.  Please remove the brackets as you go.",
                    "  |",
                    "  |  3) Repeat steps 1-2 for each ambiguous name.",
                    "  |",
                    "  |  Note: please check to make sure that you have disambiguated all name alternates",
                    "  |        by using the search function to find any stray brackets ('[ ' or ' ]')",
                    "",
                    "",
                    "  ORPHANED ALTERNATE NAMES",
                    "",
                    "    The following alternate names are orphaned.  They appear in the *Char.csv file",
                    "    but are not found in the .txt file.:",
                    "",
                    paste("    ", alt.not.found),
                    "",
                    "  Instructions: ",
                    "",
                    "  |  Orphaned names appear in the *Char.csv file but not in the .txt file.",
                    "  |  Please check each name to see that the spelling, punctuation,",
                    "  |  and capitalization patterns match between the *Char.csv and .txt files.,",
                    "  |  and correct the alternate in the *Char.csv file.",
                    "  |",
                    "  |  It is possible that some orphaned names are the result of coding issues.",
                    "  |  Please note these in the NOTES section below.",
                    "",
                    "",
                    "  NOTES (type any notes below this line; add as many lines as necessary)",
                    "",
                    "",
                    "",
                    "END ERRATA",
                    "",
                    ""
  )

  out.v <- c(out.errata.v, text.out.v)

  ## j) saves as a .txt file
  if(write.report == TRUE){
    write(out.v, file = paste0("data/", filename, "/", filename, "-textprep.txt")) # save file
    cat("File saved as", paste0(getwd(), "/data/", filename, "/", filename, "-textprep.txt", "\n"))
  }
  if(return.results == TRUE){
    txtDisambig.l <- list(ambiguous.alt = ambiguous.alt.out,
                          alt.not.found = alt.not.found.out)
    txtDisambig.l
  }
}

#txtDisambig()
