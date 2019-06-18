#' Txt_to_df()
#'
#' Along with charDf_to_edgelist(), this is one of the central utilities of the NovNet package.
#' a) scans in the .txt file of a novel, and transforms it into a vector
#' b) scans in a *Char.csv spreadsheet of character name alternates
#' c) locates all the alternate names in the .txt vector, and
#' d) replaces them with uniqnames.
#' e) it returns as output a list of objects tailored for network analysis
#'
#' @param filename Character string of text name, with associated Char.csv and .txt files.
#' @param include.all Logical vector or numerical range. If TRUE (default), processes whole text.  If range (i.e. 1:18), processes only those chapters.  If FALSE, will offer a prompt to determine a range.
#' @param set.voice NA, or character string. If NA, Char.csv pulls voice from associated *Char.csv file. If "third", will ignore first-person pronouns.  If "first", will attempt to transform all non-dialogue "I" and "me" into narrator, signaled on Char.csv file as "TextEgo".
#' @param local Logical vector.  If FALSE (default), looks in Google Drive for files.  If TRUE, looks for filename in a folder with path data/filename/.
#' @param chunk.length Numeric.  Default = 5000.  Only used if text contains no chapters.
#'
#' @keywords NovNet Utilities
#'
#' @import googledrive
#'
#' @export

Txt_to_df <- function(filename,
                      include.all = TRUE,
                      set.voice = NA, # set to override Char.csv settings
                      chunk.length = 5000, # used only if no chapters
                      local = FALSE){

  ### 1) Create local .txt file and scan into R environment
  file.v <- paste0(filename, ".txt")
  if(!dir.exists(paste0("data/", filename))){
    dir.create(paste0("data/", filename))
  }
  file.path.v <- paste0("data/", filename, "/", file.v)
  # download from Google Drive if local == FALSE
  if(local == FALSE){
    cat("Downloading .txt file from Google Drive.\n")
    drive_download(file.v, overwrite = TRUE, path = file.path.v)
  }else{
    cat(paste0("Loading file locally from: ", getwd(), "/", file.path.v, "\n"))
  }
  input.text.v <- scan(file = file.path.v, what="character", sep="\n")

  ### 2) Download local *Char.csv file and read metadata into R environment
  if(local == FALSE){
    cat("Downloading character data from Google Drive.\n")
    drive_download(file = paste0(filename, "Char.csv"),
                   overwrite = TRUE,
                   type = "csv",
                   path = paste0("data/", filename, "/", filename, "Char.csv"))
  } else {
    cat("Loading file locally from:", paste0(getwd(), "/data/", filename, "Char.csv\n"))
  }

  ## 2b) Pull file metadata from *Char.csv

  file.metadata.v <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                              header = FALSE, sep = ",",
                              skip = 0,
                              stringsAsFactors = FALSE)[1:6, 2]
  file.metadata.v[is.na(file.metadata.v)] <- "" # removes NA from completely blank columns
  names(file.metadata.v) <- c("Author", "Title", "Year", "Voice", "Compiler", "CompDate")

  ### 3) Transform input text into text.v, a vector of words

  chapter.pos.raw.v <- grep("^CHAPTER ", input.text.v)
  chapter.pos.v <- c(chapter.pos.raw.v, length(input.text.v) +1)
  # strip metadata and/or pull only consecutive range of chapters if include.all = FALSE
  if(class(include.all) == "logical"){
    if(include.all == TRUE){
      start.v <- min(which(input.text.v == "====START===="))+1
      end.v <- which(input.text.v == "====END====")-1
    } else {
      chapter.minmax.char.v <- input.text.v[c(min(chapter.pos.v), max(chapter.pos.v)-1)]
      chapter.minmax.v <- as.numeric(gsub("^CHAPTER ", "", chapter.minmax.char.v))
      cat("Start chapter: ", min(chapter.minmax.v),
          "\nFinal chapter: ", max(chapter.minmax.v),
          "\n")
      start.end.input.v <- readline("Input desired chapter range in format x:y >> ")
      if(gsub("\\d+[[:punct:]]\\d+", "correct.format", start.end.input.v) != "correct.format"){
        cat("Incorrect chapter range format; returning complete text\n")
        start.end.input.v <- as.character(paste0(min(chapter.minmax.v), "-", max(chapter.minmax.v)))
      }
      start.end.v <- as.numeric(unlist(strsplit(start.end.input.v, "\\:|\\-|\\,")))
      start.v <- chapter.pos.v[start.end.v[1]]
      end.v <- chapter.pos.v[start.end.v[2]+1]-1
    }
  }
  if(class(include.all) == "integer"){
    start.v <- chapter.pos.v[min(include.all)]
    end.v <- chapter.pos.v[max(include.all)+1]-1
  }
  main.text.v <- input.text.v[start.v:end.v]
  # Convert to single element with spaces
  text.s <- paste(main.text.v, collapse = " ")

  ## Run first_to_third(), for voice != "third"
  ## first_to_third() converts narrator tokens in 1st-person narrative to EgoName
  ## by capturing and transforming all "I", "me" outside quotations
  if(is.na(set.voice) == FALSE){
    voice <- set.voice
  }
  if(is.na(set.voice) == TRUE){
    voice <- file.metadata.v["Voice"]
  }
  if(voice == "first"){
    text.s <- first_to_third(input.text = text.s, egoname = "TextEgo")
  }
  if(voice == "epistolary"){
    text.s <- epistolary(input.text = main.text.v) # note: requires a text.v-type object
                                               # returns text.s-type object
  }
  if(voice == "frankenstein"){
    # prepare narrator.breaks df with chapter breaks and associated Ego
    start.chap <- c(1, 2, 6, 15, 21, 29)
    end.chap <- c(1, 5, 14, 20, 28, 29)
    egoname <- c("TextEgo1", "TextEgo2", "TextEgo3", "TextEgo4", "TextEgo3", "TextEgo2")
    frankenstein.narrators.df <- data.frame(start.chap, end.chap, egoname,
                                            stringsAsFactors = FALSE)
    # run complex_to_third(), which runs first_to_third() over chapters by break
    text.by.narrator.l <- complex_to_third(input.text = main.text.v,
                                           narrator.breaks = frankenstein.narrators.df)
    text.s <- do.call(paste, text.by.narrator.l)
  }
  if(voice == "winw"){
    # prepare narrator.breaks df with chapter breaks and associated Ego
    start.chap <- c(1, 16, 20, 22, 32, 33, 34, 36, 37, 38, 39, 40, 41, 52, 53, 60, 61)
    end.chap <- c(15, 19, 21, 31, 32, 33, 35, 36, 37, 38, 39, 40, 51, 52, 59, 60, 63)
    egoname <- paste0("TextEgo", c(1, 2, 3, 3, 4, 5, 6, 7, 8, 9, 11, 1, 1, 10, 1, 4, 1))
    winw.narrators.df <- data.frame(start.chap, end.chap, egoname,
                                            stringsAsFactors = FALSE)
    # run complex_to_third(), which runs first_to_third() over chapters by break
    text.by.narrator.l <- complex_to_third(input.text = main.text.v,
                                           narrator.breaks = winw.narrators.df)
    text.s <- do.call(paste, text.by.narrator.l)
  }

  # transform text.s to a vector of words, leaving in capital letters
  # note: hyphens must be left in!
  text.l <- strsplit(text.s, "[^A-z0-9_-]") # same as "\\W" but leaves in hyphens
  text.v <- unlist(text.l)
  text.v <- gsub("_", "", text.v) # remove underscore
  text.v <- text.v[which(text.v != "")]

  ### 4) Create Data Frame of Character Data ----

  ### Determine .csv Version ##
  char.data.version <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                                header = FALSE, sep = ",",
                                skip = 0,
                                stringsAsFactors = FALSE,
                                blank.lines.skip = FALSE)[7, 2]
  cat("Scanning character data from", paste0("data/", filename, "/", filename, "Char.csv\n"))
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
    char.data.df$UniqueName <- rownames(char.data.df)
  }
  if(char.data.version == "2"){
    char.data.df <- read.csv(file = paste0("data/", filename, "/", filename, "Char.csv"),
                             header = TRUE, sep = ",",
                             skip = 9,
                             stringsAsFactors = FALSE,
                             blank.lines.skip = TRUE)
    char.data.df[is.na(char.data.df)] <- "" # removes NA from completely blank columns
    rownames(char.data.df) <- char.data.df$UniqueName
    charname.col.v <- which(colnames(char.data.df) == "CharName")
    char.names.df <- char.data.df[1:nrow(char.data.df),
                                  charname.col.v:ncol(char.data.df)]
    row.names(char.names.df) <- char.data.df[1:nrow(char.data.df), 1]
  }

  ## BLOCK !AltNames from char.data.df in text.v
  # pull !AltNames from char.data.df
  # create matrix of block.alt positions in char.data.df
  block.alt.pos.m <- NULL
  for(i in 1:ncol(char.names.df)){
    block.alt.temp.v <- grep("^\\!", char.names.df[, i], value = TRUE) # names
    block.col.pos.v <- grep("^\\!", char.names.df[, i], value = FALSE) # rows
    block.alt.pos.temp.m <- cbind(block.col.pos.v, rep(i, length(block.alt.temp.v))) # matrix of format ROW, COLUMN
    rownames(block.alt.pos.temp.m) <- block.alt.temp.v
    block.alt.pos.m <- rbind(block.alt.pos.m, block.alt.pos.temp.m)
  }
  if(nrow(block.alt.pos.m) == 0){
    block.alt.pos.m <- NULL
  }
  if(!is.null(block.alt.pos.m)){
    colnames(block.alt.pos.m) <- c("row", "column")
    # build data frame of blocked names
    block.alt.df <- as.data.frame(block.alt.pos.m)
    block.alt.df$block.alt <- gsub("^!", "", rownames(block.alt.pos.m))
    block.alt.df$regex <- paste("\\W", # non-word character
                                block.alt.df$block.alt,
                                "\\W", # non-word character
                                sep = "") # note: \\< and \\> don't seem to work because it doesn't recognize Mr. B---- as a "word"
    # remove punctuation from $regex, so it matches with text.v
    block.alt.df$regex <- gsub("\\.|\\,", "", block.alt.df$regex)
    block.alt.df$regex <- gsub("\\'", " ", block.alt.df$regex) # the problem of Irish names (i.e. O'Rourke)
    # create tags, to find and transform back later
    block.alt.df$tag <- paste0("BlockedALT", 1:nrow(block.alt.df))
    # NOTE: block.alt.df is used below, after text.uniq.s is created
    ## DELETE blocked alts from char.names.df
    for(j in 1:nrow(block.alt.df)){
      char.names.df[block.alt.df$row[j], block.alt.df$column[j]] <- ""
    }
  }

  ## Transform names in text.v to uniquenames-- using gsub()
  cat("Correlating character alternate names with uniqnames\n")
  ## Create Regex pattern of name and name alternates
  # find positions which contain names and name alternates
  alt.pos.v <- which(char.names.df != "", arr.ind = TRUE) #returns matrix in format:
              #               row       column
              # uniqname
  # bind each alternate name (recorded as [r,c] position in alt.pos.v) to rowname
  # in new two-column matrix, so each alternate name is bound to its uniqname
  alt.uniq.pair <- cbind(rownames(alt.pos.v), char.names.df[alt.pos.v])
              #               uniqname  alternatename
              # [1,]

  ## Reorder alt.uniq.pair rows from longest ngram to shortest ngram
  ordered.pair.m <- alt.uniq.pair[order(sapply(strsplit(alt.uniq.pair[, 2], " "), length),
        decreasing = TRUE), ]
  # strip whitespace from names in ordered.pair.m
    ordered.pair.m <- gsub(" $", "", ordered.pair.m)
    ordered.pair.m <- gsub("^ ", "", ordered.pair.m)
  # transform to data frame
  ordered.pair.df <- as.data.frame(ordered.pair.m, stringsAsFactors = FALSE)
  colnames(ordered.pair.df) <- c("uniqname", "alt")

  ## Transform alternate names to Regular Expressions in ordered.pair.df
  ordered.regex.v <- paste("\\W", # non-word character
                           ordered.pair.df$alt,
                           "\\W", # non-word character
                           sep = "") # note: \\< and \\> don't seem to work because it doesn't recognize Mr. B---- as a "word"
  # remove punctuation from ordered.regex.v, so it matches with text.v
  ordered.regex.v <- gsub("\\.|\\,", "", ordered.regex.v)
  ordered.regex.v <- gsub("\\'", " ", ordered.regex.v) # the problem of Irish names (i.e. O'Rourke)

  # Bind ordered.regex.v to ordered.pair.df$regex
  ordered.pair.df$regex <- ordered.regex.v

  ### 3) Prepare text.uniq.s: a character string with character names transformed to uniqnames

  text.uniq.s <- paste(text.v, collapse = " ")

  ## Block !alt.block
  if(!is.null(block.alt.pos.m)){
    for(i in 1:nrow(block.alt.df)){
      text.uniq.s <- gsub(block.alt.df$regex[i], block.alt.df$tag[i], text.uniq.s)
    }
  }
  # NOTE: this will be reversed after finding names below

  ## replace names and name alternates with uniqnames
  cat("Replacing names and name alternates with uniqnames.
      This might take a while..")
  hash.length.v <- length(ordered.pair.df$regex)/10
  prog.hash.v <- round(seq(from = 0,
                           to = length(ordered.pair.df$regex),
                           by = hash.length.v), digits = 0)
  prog.hash.ticker <- 2
  alt.not.found <- NULL
    for(i in 1:length(ordered.pair.df$regex)){
      if(length(grep(ordered.pair.df$regex[i], text.uniq.s)) == 0){
        alt.not.found <- c(alt.not.found, ordered.pair.df$alt[i])
      }
      text.uniq.s <- gsub(ordered.pair.df$regex[i],
                          paste0(" ", ordered.pair.df$uniqname[i], " "),
                          text.uniq.s)

      if(i == prog.hash.v[prog.hash.ticker]){
        prog.hash.ticker <- prog.hash.ticker + 1
        cat("..")
      }
    }
  cat("\n")
  ## reverse block.alt (from above), removing sticky block.alt names

  if(!is.null(block.alt.pos.m)){
    for(i in 1:nrow(block.alt.df)){
      text.uniq.s <- gsub(block.alt.df$tag[i], block.alt.df$block.alt[i], text.uniq.s)
    }
  }

  text.uniq.v <- unlist(strsplit(text.uniq.s, "[^A-z0-9_-]"))
  text.uniq.v <- text.uniq.v[which(text.uniq.v != "")]

  ### 4) sort char.data.df by total character name appearances, from
  # least (row1) to most (row(nrow(df)))
  cat("Sorting char.data.df by total character uniqname token counts\n")
  char.count.v <- NULL
  for(i in 1:length(rownames(char.data.df))){
    char.count.v[rownames(char.data.df)[i]] <- length(which(text.uniq.v == rownames(char.data.df)[i]))
  }
  # order char.data.df by character popularity
  char.data.df$tokenCount <- char.count.v
  char.data.df <- char.data.df[order(char.data.df$tokenCount), ]
  # and create vector of character names dramatis.personae.v
  dramatis.personae.v <- rownames(char.data.df)

  text.chaps.l <- list()
  table.chaps.l <- list()

  ### 5) store chapter breaks -- and fix fencepost problem
  cat("Creating list of chapters and list of token frequency tables (by chapter)\n")
  chapter.breaks.v <- c(grep("CHAPTER", text.uniq.v), length(text.uniq.v) + 1)
  if(length(chapter.breaks.v) <= 1){
    chapter.breaks.v <- seq(from = 1,
                            to = (length(text.uniq.v)+1),
                            by = chunk.length)
  cat("No chapter breaks found.\n",
    "Total word count:", length(text.uniq.v), "\n",
    "Generating", length(chapter.breaks.v), "arbitrary chunks of max", chunk.length, "words each.\n",
    "Specify non-default chunk length by defining variable chunk.length.v.\n")
  }
  for(i in 1:(length(chapter.breaks.v)-1)){
    chap.start.v <- chapter.breaks.v[i]
    chap.end.v <- chapter.breaks.v[i+1]-1
    temp.text.v <- text.uniq.v[chap.start.v:chap.end.v]
    text.chaps.l[[i]] <- temp.text.v
    table.chaps.l[[i]] <- table(temp.text.v)
  }

  ### 6) Create blank list and populate with data to return
  Txt_to_df.l <- list()
  objects.to.return <- c("char.data.df",
                         "text.v", "text.uniq.v",
                         "text.chaps.l", "table.chaps.l",
                         "dramatis.personae.v", "alt.not.found",
                         "file.metadata.v") # note: if
                        # alt.not.found is NULL, it will not be returned below
  for(l in 1:length(objects.to.return)){
    object.name <- objects.to.return[l]
    Txt_to_df.l[[object.name]] <- get(object.name)
  }

  cat("\n",
      "Output list contains these objects:\n")
  for(name.v in names(Txt_to_df.l)){
    assign(name.v, Txt_to_df.l[[name.v]])
    cat(paste(which(names(Txt_to_df.l) == name.v), name.v, "\n", sep = " "))
  }
  if(length(alt.not.found) > 0){
    cat("\nNOTE:", length(alt.not.found), "character alternate names were not found.
      call $alt.not.found for a vector of absent alternate names.\n")
  }
  if(length(alt.not.found) == 0){
    alt.not.found <- "All character alternate names were located in the text."
  }
  Txt_to_df.l
}


# rm(list = c("Txt_to_df.l", "Txt_to_df"))

# char.data.df, a data frame of character names and name alternates
# text.v, a vector of words, capitalization preserved, punctuation removed
# text.uniq.v, text.v with names transformed to uniqnames
# text.uniq.s, text.v as a string, with names transformed to sticky names
# dramatis.personae.v an ordered sequence of uniqnames, sorted by token count
# two lists derived from text.uniq.v: text.chaps.l and table.chaps.l")

