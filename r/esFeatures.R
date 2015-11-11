esFeatures <-
function(es, features='',column_lookup='gene_symbol', anno= annot) {
  ############################################
  ## Function 11
  ## Function that generates information on the features selected
  ##  es - expression set
  ##  features - typically genes or anything found in the column of annotation needed
  ##  column_lookup - typically gene_symbol but can be anything in the annotation file
  ## returns list with multiple information,
  ##  first vector is the featureNames of the es matching features
  ##  second vector is the features and featureNames pasted to gether with '__'
  #############################################
  annot<-NULL #had to set to NULL to pass R CMD Check
  
  #3/1/14 changed the argument to anno=annot, which was having problems with the R CMD check.  Anno is the object that is handled
  #by this function
  
  column_lookup = colnames(anno)[grep(column_lookup, colnames(anno))]
  column_lookup = column_lookup[[1]]
  sprintf(paste0("Choosing column: ", column_lookup))
  anno_idx = unlist(lapply(features, function(x) which(x == anno[[column_lookup]])))  
  ids = anno[anno_idx,][[1]]
  #sprintf(as.character(ids))
  es_idx = sapply(unlist(ids), function(x) which(x == featureNames(es)))
  es_idx2 = paste(unlist(anno[anno_idx,][column_lookup]), ids, sep="__")
  list(es_idx, es_idx2, anno[anno_idx,])
}
