esLoadTarget <-
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
  annotation_HuEx_tsr<-NULL #setting the variable to NULL to pass R CMD check 
  anno<-NULL 
  annotation_HuEx_psr<-NULL 
  
  
  if(is.null(type) | !(type %in% c('tsr', 'psr', 'avgaffy', 'avgref')) ) {
    cat("Use type=  (tsr or psr or avgaffy or avgref)
        tsr: transcript\npsr-probeset\navgaffy-average transcript based on Affy annotation                   \navgref: average transcript based on Refgene")
    return()
  }
  if (type=="tsr") {
    data_file= ".//data//target//TARGET_core_tsr_expressionset.Rdata"
    annofile = ".//Annotations//huex_anno_tsr.Rdata"
    load(file=fixpath(data_file))
    load(file=fixpath(annofile))
    anno = annotation_HuEx_tsr[which(annotation_HuEx_tsr$transcript_cluster_id %in% featureNames(eset)),]    
    rm(annotation_HuEx_tsr)
    annot = anno[c(1, grep("gene_symbol", colnames(anno)))]
  }
  if (type=="psr") {
    data_file = ".//data//target//TARGET_core_psr_expressionset_102312.Rdata"
    annofile = ".//Annotations//huex_anno_psr.Rdata"
    load(file=fixpath(data_file))
    load(file=fixpath(annofile))
    anno = annotation_HuEx_psr[which(annotation_HuEx_psr$probeset_id %in% featureNames(eset)),]    
    rm(annotation_HuEx_psr)
    annot = anno[c(1, grep("gene_symbol", colnames(anno)))]
  }
  if (type=="avgaffy"){
    data_file = ".//data//target//TARGET_core_avgaffy_expressionset.Rdata"
    annofile = ".//Annotations//huex_anno_tsr.Rdata"
    load(file=fixpath(data_file))
    load(file=fixpath(annofile))
    anno = annotation_HuEx_tsr
    anno[[1]] = anno$gene_symbol
    anno = anno[which(!duplicated(anno[[1]])),]
    anno = anno[which(anno$gene_symbol %in% featureNames(eset)),]    
    rm(annotation_HuEx_tsr)
    annot = anno[c(1, grep("ref_gene_symbol", colnames(anno)))]
  }
  if (type=="avgref"){
    data_file = ".//data//target//TARGET_core_avgref_expressionset.Rdata"
    annofile = ".//Annotations//huex_anno_tsr.Rdata"
    load(data_file)
    load(annofile)
    anno = annotation_HuEx_tsr
    anno$transcript_cluster_id = anno$ref_gene_symbol
    anno = anno[which(!duplicated(anno$ref_gene_symbol)),]
    anno = anno[which(anno$ref_gene_symbol %in% featureNames(eset)),]    
    rm(annotation_HuEx_tsr)
    annot = anno[c(1, grep("ref_gene_symbol", colnames(anno)))]
  }
  
  
  #########################
  ## Remove Chromosome Y data and samples not needed for TARGET
  ## Remove cell lines, 4S samples and one outlier 92028
  #########################
  if(excludeYprobes) {
    nonYfeatures = anno[!anno$seqname %in% c('chrY'),]$transcript_cluster_id
    if(typeof(featureNames(eset))=='character') nonYfeatures= as.character(nonYfeatures)
    eset = eset[nonYfeatures,]
  }
  
  if (load_target_subset) {
    #eset = eset[,eset[['CHLA.Number']]!=92028]  excluded this from the psr data.
    eset = eset[,eset[['stage']] !='4s']
    eset = eset[,eset[['stage']] !='4S']
    eset = eset[,eset[['tumor_or_normal']]=='tumor']
    #eset = eset[,eset[['risk_a']]%in%c('cell line', 'HRA', 'HRN', 'LR', 'IR')]
    #eset = eset[,eset[["outlier"]] =="No"]
  }
  
  ### Expressionset that will be used//modified:
  return(list(eset = eset, anno = anno, annot = annot))
}
