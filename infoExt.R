##This is used to extract information form the output of several open-source optimization solvers

#install.packages("stringr")
#install.packages("stringi")
library(stringr)
library(stringi)

####get the name list of the .mps files (in a data frame)####
getNameList <- function(mypath, filename)
{
  setwd(mypath)
  namelist <- read.table(file = filename, col.names = "Name")
  namelist$Name <- gsub('.{4}$', '', namelist$Name)
  return(namelist)
}

lpList <- getNameList("F:\\testproblems\\test1\\MPS", "lp_namelist.txt")
mipList <- getNameList("F:\\testproblems\\miplib2010_51", "mip_namelist.txt")

setwd("F:\\Result")