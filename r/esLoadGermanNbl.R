#' esLoadGermanNbl
#' 
#' Load Function and automatically load target data. Then, removes Y chromosome
#' features.
#' 
#' 
#' @param type 4 types: tsr (transcript), psr (probe script), avgaffy (average
#' transcript base on affy notation), avgref (average transcript based on ref
#' gene)
#' @param load_target_subset takes a different annotation r-object dataset
#' @param excludeYprobes Logical argument.If \code{TRUE}, it will remove the
#' cell lines. Default to \code{TRUE}. It will return to the expression set.It
#' removes probeset that belongs to chromosome y.
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #esLoadGermanNbl(type = NULL, load_target_subset = TRUE, excludeYprobes = TRUE)
#' 
esLoadGermanNbl <-
function(type = NULL, load_target_subset=TRUE, excludeYprobes=TRUE) {
  ############################
  ## Load Function 
  ## Automatically load target data
  ## Removes Y chromosome features
  ###########################
  
  ############################################################
  ## Load the necessary extra functions written by shahab
  ## And set output dir for figures
  ############################################################
  #set_tmp_path()
  outputdir = ".//Paper//Figures//tsr//"
  
  #############################################################
  ### Part 1
  ### Load expression profile and annotation file
  ### Build expressionset class and annotate featureNames
  ### Uses function (1)
  #############################################################
  if(is.null(type) | !(type %in% c('tsr', 'psr', 'avgaffy', 'avgref')) ) {
    cat("Use type=  (tsr or psr or avgaffy or avgref)
        tsr: transcript\npsr-probeset\navgaffy-average transcript based on Affy annotation                   \navgref: average transcript based on Refgene")
    return()
  }
  if (type=="tsr") {
    data_file = ".//data//german//german_expressionset.Rdata"
    annofile = ".//data//german//Agilent_annotations.Rdata"   # This is from Jana using NIH David
    #annofile = ".//Annotations//Agilent//Annotation_from_Agilent.Rdata"  # This is from Agilent
    load(file=fixpath(annofile))
    load(file=fixpath(data_file))
    anno = anno[which(anno$probeset_id %in% featureNames(eset)),]   
    annot = anno[c(1, grep("gene_symbol", colnames(anno)))]
  }
  if (type=="avgref"){
    data_file = ".//data//german//german_expressionset_avgref.Rdata"
    annofile = ".//data//german//Agilent_annotations.Rdata"
    load(data_file)
    load(annofile)
    anno$probeset_id = anno$gene_symbol
    anno = anno[which(!duplicated(anno$gene_symbol)),]
    anno = anno[which(anno$gene_symbol %in% featureNames(eset)),]    
    annot = anno[c(1, grep("gene_symbol", colnames(anno)))]
  }
  if (type=="pctile"){
    data_file = ".//data//german//german_expressionset_pctile.Rdata"
    annofile = ".//data//german//Agilent_annotations.Rdata"
    load(file=fixpath(data_file))
    load(file=fixpath(annofile))
    anno$probeset_id = anno$gene_symbol
    anno = anno[which(!duplicated(anno$gene_symbol)),]
    anno = anno[which(anno$gene_symbol %in% featureNames(eset)),]    
    annot = anno[c(1, grep("gene_symbol", colnames(anno)))]
  }
  

  #########################
  ## Remove Chromosome Y data  
  #########################
  if(excludeYprobes) {
    nonYfeatures = anno[!anno$seqname %in% c('chrY'),]$probeset_id
    if(typeof(featureNames(eset))=='character') nonYfeatures= as.character(nonYfeatures)
    eset = eset[nonYfeatures,]
  }
  
  
  #############
  ## Exclude 4S 
  #############
  
  if (load_target_subset) {
    eset = eset[,eset[['inss_stage']] != '4S']
  }
  
  annot = anno[c(1, grep("gene_symbol", colnames(anno)))]
  return(list(eset = eset, anno = anno, annot = annot))
}
