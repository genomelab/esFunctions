#the purpose of this script is to run a test_that script using the testthat package on the 
#esFindgenecor script which  I had changed in order to pass R CMD check.

#this will test that all the right objects passed into the functions are the correct classes.


#note that all the tests must be saved in the packaged inst/tests.
setwd("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/R Package/esFunctions/inst/tests")


source("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/R Package/esFunctions/R/esFindgenecor.R")
source("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/R Package/esFunctions/R/esFeatures.R")
source("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/R Package/esFunctions/R/esAnnotate.R")
source("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/Anthony/Data Microarray Eset Mice/Genome_Studio_To_Eset/Eset Scripts for GS Project/GS_To_Eset.R")

#Source in the eset used for Mice, as a test eset.



test_that("Correct Classes", {
  
  
  
  
})