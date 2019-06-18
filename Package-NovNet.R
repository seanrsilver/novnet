#### NovNet Package
#### This script is designed to generate a package of functions for
#### Novel Networks
#### NOTE: this script is adapted from:
####       https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
####  and:
####       https://kbroman.org/pkg_primer/pages/docs.html

setwd("~/Box/RLibrary/NovelNetworks")

packages.v <- c("devtools", "roxygen2") # a list of packages to install() and library()

# a for () loop to install all packages in packages.v
for(package in packages.v){
  if(!require(package, character.only = TRUE)){
    install.packages(package)
    library(package, character.only = TRUE)
  }
}
rm(list = c("packages.v", "package"))

# create_package(path = "NovNet") #builds blank NAMESPACE, DESCRIPTION (etc...) files

setwd("~/Box/RLibrary/NovelNetworks/NovNet")

build(path = paste0(getwd()))
document()
updatePackageVersion <- function(packageLocation ="."){
  ## Read DESCRIPTION file
  desc <- readLines(file.path(packageLocation, "DESCRIPTION"))
  ## Find the line where the version is defined
  vLine <- grep("^Version\\:", desc)
  ## Extract version number
  vNumber <- gsub("^Version\\:\\s*", "", desc[vLine])
  ## Split the version number into two; a piece to keep, a piece to increment
  versionNumber <- strsplit(vNumber, "\\.")[[1]]
  versionParts <- length(versionNumber)
  vNumberKeep <- paste(versionNumber[1:(versionParts-1)], sep= "", collapse= ".")
  vNumberUpdate <- versionNumber[versionParts]
  ## Replace old version number with new one (increment by 1)
  oldVersion <- as.numeric(vNumberUpdate)
  newVersion <- oldVersion + 1
  ## Build final version number
  vFinal <- paste(vNumberKeep, newVersion, sep = ".")
  ## Update DESCRIPTION file (in R)
  desc[vLine] <- paste0("Version: ", vFinal )
  ## Update the actual DESCRIPTION file
  writeLines(desc, file.path(packageLocation, "DESCRIPTION"))
  ## Return the updated version number to screen
  return(vFinal)
}
updatePackageVersion()
check() # note: if "dependency not required" error, add name of package to description.
install(pkg = ".", reload = TRUE)

# to attach:
