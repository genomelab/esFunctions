esAnnotate <-
function(es, annot) {
  #############################################################
  ## Function 3
  ## esAnnotate
  ## Annotates porbeset_id or transcript_id to gene name
  ##  es - expressionset object
  ##  Annot - Annotation object (first column is the probe_id)
  ##  Returns expressionset object that has been reAnnotated  
  ##  (i.e. featureNames have been rewritten)
  ##  Assumes probeset_id//transcript_id is in column one of the Annotation 
  ##  and column two is the gene_symbol//value of 
  ############################################################
  features = as.data.frame(featureNames(es))
  features = cbind(features, c(1:dim(features)[1]))
  fannot = as.data.frame(annot)
  newfeatures = merge(features, annot, by.x = colnames(features)[1], by.y = colnames(annot)[1], all.x = TRUE, sort=FALSE)
  newfeatures = newfeatures[order(newfeatures[2]),]
  # only one of the porbes for probes with more than one gene are kept
  #newfeatures2 = newfeatures[!duplicated(newfeatures[1]),]
  newfeatures = cbind(newfeatures, paste(newfeatures[,3],"__",newfeatures[,1], sep=""))  
  featureNames(es) = as.character(newfeatures[,length(newfeatures)])
  es
}
