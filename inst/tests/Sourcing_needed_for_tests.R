source("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/R Package/esFunctions/R/esAnnotate.R")
source("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/R Package/esFunctions/inst/tests/eset.R")
#the above source loads eset.R and all the object related;  so eset has rownames as transcript ID's and colnames as pData.
#esAnnotate transforms transcript ID's to gene symbol from object anno.
library("testthat")

setwd("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/R Package/esFunctions/inst/tests")
