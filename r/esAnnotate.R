#' esAnnotate
#' 
#' esAnnotate annotates probeset_id or transcript_id to gene name.  In this
#' example we had an R object which was a large data set called
#' annotation_Huex_tsr that had columns names of unique Transcript_cluster_ids,
#' probset_id, seqname, strand, start and stop locations, and ref_gene_symbols.
#' The expression set must already be made, and have feature names as
#' transcipt_ids, and an annotation data set must already be used as an input.
#' The esAnnotate function will create a new data frame by eliminating the
#' postscriptIDs, and merge the new dataframe to the expression set linking
#' together the transcript IDs.  After the transcript IDs are linked, the first
#' row is deleted which leaves the new "first" row as the gene names.  This
#' makes the featureNames as the gene names and not the transcript IDs.
#' esAnnotate does not average the duplicates, but keeps all the gene names as
#' independent entries and keeps the transcript ID as a subscript.
#' 
#' 
#' @param es expression set object
#' @param annot is an annotation object (first column is the probe_id). Returns
#' expression set object that has been reannotated (i.e. featureNames have been
#' rewritten). Assumes probeset_id//transcript_id is in column one of the
#' annotation and column two is the gene_symbol//value of
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' ##Note the Huex_anno_tsr.Rda\cr
#' #infilepath<-"C:/Users/GENERAL PATH TO YOUR ANNOTATION SET/huex_anno_tsr.Rdata"\cr
#' #outfilepath<-"C:/Users/GENERAL PATH TO ANNO SET/huex_anno_tsr.txt"\cr
#' #load(infilepath) #loading the Huex Annotation data set\cr
#' 
#' #removing the postscriptIDs and all of the other data\cr
#' #annotrans<-annotation_HuEx_tsr$transcript_cluster_id\cr
#' #anno<-annotation_HuEx_tsr[,c(1,20)] #grabbing the only the columns with #transcript IDs and\cr Reference Gene symbols, and ommitting the rest.\cr
#' #class(anno)\cr
#' #View(anno) #see that the all the data is removed except for the #transcriptID's and the ref_gene_symbol\cr
#' #now the annotated data frame is created, and need to attach this to #matching IDs in the eset\cr
#' 
#' #checking the features of eset, they are transcript IDs (unique) need #to change to gene name\cr
#' #featureNames(eset)\cr
#' 
#' #The annotation set and feature names must be a data frame.\cr
#' #feature<- as.data.frame(featureNames(eset))\cr
#' #feature<-cbind(feature, c(1:dim(feature)[1])) \cr 
#' #frameannot<-as.data.frame(anno)\cr
#' 
#' #Annotatedeset<-esAnnotate(eset,annot)\cr
#' #featureNames(Annotatedeset)\cr
#' 
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
