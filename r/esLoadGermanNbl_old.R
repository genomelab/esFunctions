esLoadGermanNbl_old <-
function(type = "tsr",  load_target_subset=TRUE, excludeYprobes=TRUE) {
  
  ############################
  ## Function 26
  ## Automatically load German Neuroblastoma data
  ## load_target_subset = TRUE, excludes 4S cases
  ###########################
  #set_tmp_path()
  data_file = ".//data//german//german_expressionset.Rdata"
  annofile = ".//data//german//Agilent_annotations.Rdata"   # This is from Jana using NIH David
  #annofile = ".//Annotations//Agilent//Annotation_from_Agilent.Rdata"  # This is from Agilent
  load(file=fixpath(annofile))
  load(file=fixpath(data_file))
  
  anno = anno[which(anno$probeset_id %in% featureNames(eset)),]   
  
  
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
