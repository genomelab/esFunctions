#this is a test for esAnnotate
setwd("/Users/anthonycolombo60/Dropbox/TARGET PROJECT/R Package/esFunctions/inst/tests")
library("testthat")

#now to test esAnnotate
test_that("Correct Class", {
  Antd_eset<-esAnnotate(eset,anno) #these objects are generated from eset.R data
  expect_that(Antd_eset, is_a("ExpressionSet"))
  expect_that(anno, is_a("data.frame"))
  expect_that(eset, is_a("ExpressionSet"))
  expect_that(rownames(anno), is_a("character"))
  expect_that(featureNames(eset), is_a("character"))
  
})

#all the class objects work,  how else to test this function?

