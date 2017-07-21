#' esLoadMedullo
#' 
#' Automatically load medulloblastoma data, removes chr Y features
#' 
#' 
#' @param type loads transcripts without cell lines
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #medullo <- esLoadMedullo(type="tsr")
#' 
esLoadMedullo <- function(type = "tsr") {
  ############################
  ## Function 20
  ## Automatically load medulloblastoma data, removes chr Y features
  ###########################
  set_tmp_path()
  ############################################################
  ## Load the necessary extra functions written by shahab
  ## And set output dir for figures
  ############################################################
  #outputdir = ".//Paper//Figures//tsr//"
  
  #############################################################
  ### Part 1
  ### Load expression profile and annotation file
  ### Build expressionset class and annotate featureNames
  ### Uses function (1)
  #############################################################
  annotation_HuEx_tsr<-NULL
  annotation_HuEx_psr<-NULL
  
  
  if (type=="tsr") {    
    data_file= ".//Medulloblastoma//Medulloblastoma_core_tsr_expressionset_080112.Rdata"
    data_file = ".//Medulloblastoma//Medulloblastoma_core_tsr_expressionset_03132013.Rdata"
    #annofile = "~//target//data//huex_anno_tsr.Rdata"
    annofile = ".//Annotations//huex_anno_tsr.Rdata"
    load(file=fixpath(data_file))
    load(file=fixpath(annofile))
    
    anno = annotation_HuEx_tsr[which(annotation_HuEx_tsr$transcript_cluster_id %in% featureNames(eset)),]
    rm(annotation_HuEx_tsr)
    nonYfeatures = anno[!anno$seqname %in% c('chrY'),]$transcript_cluster_id
    
  }
  if (type=="psr") {
    data_file = ".//Medulloblastoma//Medulloblastoma_core_psr_expressionset_080112.Rdata"
    #annofile = "~//target//data//huex_anno_psr.Rdata"
    annofile = ".//Annotations//huex_anno_psr.Rdata"
    load(file=fixpath(data_file))
    load(file=fixpath(annofile))
    anno = annotation_HuEx_psr[which(annotation_HuEx_psr$probeset_id %in% featureNames(eset)),]
    rm(annotation_HuEx_psr)
    nonYfeatures = anno[!anno$seqname %in% c('chrY'),]$probeset_id
  }
  annot = anno[c(1, grep("gene_symbol", colnames(anno)))]
  #########################
  ## Remove Chromosome Y data and samples not needed for TARGET
  ## Remove cell lines, 4S samples and one outlier 92028
  #########################
  eset = eset[nonYfeatures,]
  #eset = eset[,eset[['risk_a']]%in%c('cell line', 'HRA', 'HRN', 'LR', 'IR')]
  #eset = eset[,eset[["outlier"]] =="No"]
  
  ### Expressionset that will be used//modified:
  return(list(eset = eset, anno = anno, annot = annot))
}
