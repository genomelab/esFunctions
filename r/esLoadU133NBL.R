#' esLoadU133NBL
#' 
#' loaded data from U133 NBL
#' 
#' @export
#' @param type this is the type of load, tsr is default
#' @param excludeYprobes logical command for excluding certain probes in Y
#' @author Shahab Asgharzadeh
esLoadU133NBL <-
function(type = "tsr", excludeYprobes=TRUE) {
  ############################
  ## Automatically load U133 Neuroblastoma data
  ###########################
  
  set_tmp_path()
  data_file = ".//data//u133//u133AB_expressionset_09172012.Rdata"
  #data_file = ".//data//u133//u133A_expressionset_09172012.Rdata"
  annofile = ".//Annotations//u133//u133AandB_Complete_Annotation.Rdata"  
  load(file=fixpath(annofile))
  load(file=fixpath(data_file))
  
  colnames(anno)[1] = 'probeset_id'
  colnames(anno)[grep("Gene.Symbol", colnames(anno))] = 'gene_symbol'
  anno = anno[!duplicated(anno$probeset_id),]  #in case there are some duplicates in the annotation
  anno = anno[which(anno$probeset_id %in% featureNames(eset)),]
  
  
  #########################
  ## Remove Chromosome Y data  
  #########################
  if(excludeYprobes) {
    chrYfeatures = anno[anno$Chromosomal.Location[grep('chrY', anno$Chromosomal.Location)],]$probeset_id
    eset = eset[!(featureNames(eset) %in% chrYfeatures),]
  }
  
  annot = anno[c(1, grep("gene_symbol", colnames(anno)))]
  
  return(list(eset = eset, anno = anno, annot = annot))
}
