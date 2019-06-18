#'  scrapeTags()
#'    a) reads a file with NovNet-style tags
#'    b) scrapes the alternate names
#'    c) writing to a Char.csv file
#'    d) It will also repopulate the -scrapeTags.csv with metadata from
#'       an associated Char.csv file, if one exists.
#'
#' description
#'
#' @param filename Character string of text name, with associated -tagNames file.
#' @param local Logical vector.  If FALSE (default), looks in Google Drive for files.  If TRUE, looks for filename in a folder with path data/filename/.
#' @param include.all Logical vector or numerical range. If TRUE (default), processes whole text.  If range (i.e. 1:18), processes only those chapters.  If FALSE, will offer a prompt to determine a range.
#'
#' @keywords NovNet Utilities PhaseII
#'
#' @import googledrive
#'
#' @export
#'
#'

### scrapeTags() ----

scrapeTags <- function(filename,
                       local = FALSE,
                       include.all = TRUE){

  ## a)
  # pull -tagNames.txt file

  file.v <- paste0(filename, "-tagNames.txt")
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
  # Determine .csv Version #
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

  ## e) locate tags in text.s and scrape, saving to scraped.pair.df:
  ##
  ##                  alt         uniq
  ##    [1,]
  ##    [2,]
  # scrape tags
  scraped.tags.v <- unlist(regmatches(text.s, gregexpr("<.+?/>", text.s)))

  # check tag format
  tag.check.v <- grep("< .+ u: .+ />", scraped.tags.v, value = TRUE)
  problem.tags.v <- setdiff(scraped.tags.v, tag.check.v)
  # remove malformed tags
  scraped.tags.v <- tag.check.v

  # report malformed tags
  if(length(problem.tags.v) >= 1){
    cat(paste0(length(problem.tags.v), " malformed tags found and not collated:\n"))
    for(i in 1:length(problem.tags.v)){
      cat(problem.tags.v[i])
      cat("\n")
    }
  } else {
    cat("No malformed tags found.\n")
  }

  # split tags
  alt.as.found <- gsub("< (.+) u: (.+) />", "\\1", scraped.tags.v)
  uniq.as.found <- gsub("< (.+) u: (.+) />", "\\2", scraped.tags.v)

  # combine in data.frame
  scraped.pair.df <- data.frame(alt = alt.as.found,
                                uniq = uniq.as.found,
                                stringsAsFactors = FALSE)

  ## f) stash scraped.pair.df$alt.as.found in list, organized by scraped.pair.df$uniq.as.found
  # create vector of uniqnames, and compare to Char.csv rownames
  unique.uniq.as.found <- unique(scraped.pair.df$uniq)


  # transform vector of compiled uniqnames to list, and stash associated alternates
  found.names.l <- as.list(rep(NA, length(unique.uniq.as.found)))
  names(found.names.l) <- unique.uniq.as.found
  for(i in 1:length(found.names.l)){
    temp.uniq.v <- unique.uniq.as.found[i]
    assoc.alt.names.v <- scraped.pair.df[which(scraped.pair.df$uniq == temp.uniq.v), 1]
    found.names.l[[i]] <- unique(assoc.alt.names.v)
  }

  ## f) compare compiled alt names with Char.csv, and add where necessary
  # pull uniqnames from char.names.df, and generate list of collated alternates
  char.csv.uniq <- rownames(char.names.df)
  new.uniq.v <- unique.uniq.as.found[!unique.uniq.as.found %in% char.csv.uniq]
  ######## ANNOUNCE NEW UNIQUE NAMES ########
  if(length(new.uniq.v >= 1)){
    cat("New uniqnames found:\n", paste0(new.uniq.v, "\n"), "\n")
  }
  uniq.v <- c(char.csv.uniq, new.uniq.v)
  char.names.l <- as.list(rep("", length(uniq.v)))
  names(char.names.l) <- uniq.v
  for(i in 1:nrow(char.names.df)){
    temp.uniq.v <- uniq.v[i]
    alts.from.csv <- as.character(char.names.df[temp.uniq.v, ])
    alts.from.csv <- alts.from.csv[which(alts.from.csv != "")]
    char.names.l[[temp.uniq.v]] <- alts.from.csv
  }

  # now compare with found names, and add to char.names.l if necessary
  new.alt.found <- list()

  for(i in 1:length(char.names.l)){
    temp.uniq.v <- names(char.names.l)[i]
    new.alt.names.byuniq.v <- found.names.l[[temp.uniq.v]][!found.names.l[[temp.uniq.v]]
                                 %in% char.names.l[[temp.uniq.v]]]
    char.names.l[[i]] <- c(char.names.l[[i]], new.alt.names.byuniq.v)
    if(length(new.alt.names.byuniq.v) >= 1){
      new.alt.found[[temp.uniq.v]] <- new.alt.names.byuniq.v
    }
  }
  if(length(new.alt.found) >= 1){
    cat(paste0(sum(sapply(new.alt.found, length)), " new alternate names found.\n"))
        for(i in 1:length(new.alt.found)){
          cat(paste("uniqname", names(new.alt.found)[i], "now associated with:", new.alt.found[[i]], "\n"))
        }
  }

  ## g) prepare out.df, in format Char.csv
  nrow.out.df.v <- length(char.names.l) # 10 lines metadata plus number of characters
  ncol.out.df.v <- 5+max(sapply(char.names.l, length))
  out.m <- matrix("",
                  ncol = ncol.out.df.v,
                  nrow = nrow.out.df.v)
  out.df <- as.data.frame(out.m, stringsAsFactors = FALSE)
  out.df[1:10, 1:11] <- char.data.df[1:10, 1:11]

  # transfer char.names.l to out.df[11:nrow, ]
  for(i in 1:length(char.names.l)){
    uniq.temp.v <- names(char.names.l)[i]
    alts.temp.v <- char.names.l[[uniq.temp.v]]
    if(!uniq.temp.v %in% new.uniq.v){
      char.meta.v <- as.character(char.data.df[which(char.data.df[, 1] == uniq.temp.v), 2:5])
    } else {
      char.meta.v <- rep("", 4)
    }
    temp.row.v <- c(uniq.temp.v, char.meta.v, alts.temp.v)
    out.df[10+i, 1:(5+length(alts.temp.v))] <- temp.row.v
  }

  # add compiler tag
  out.df[7, 4] <- paste0("This Char.csv file was prepared with scrapeTag() on ", date())

  ## h) write out .csv

  write.table(out.df, # note: write.csv is inflexible on column names
              sep = ",",
              file = paste0("data/", filename, "/", filename, "Char-scrapeTag.csv"),
              row.names = FALSE,
              col.names = FALSE)

  cat("file saved as:",  paste0("data/", filename, "/", filename, "Char-scrapeTag.csv\n"))








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

  write(out.v, file = paste0("data/", filename, "/", filename, "-scrapeTags.txt")) # save file
  cat("File saved as", paste0(getwd(), "/data/", filename, "/", filename, "-scrapeTags.txt", "\n"))
}


