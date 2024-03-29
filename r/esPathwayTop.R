#' esPathwayTop
#' 
#' Finds Top scoring Pathways (using Score Calculated by multiplying pathway
#' eucledian distance and SD of Chi2.
#' 
#' @export
#' @param esPathwaySig_results pathway analysis results from
#' \code{esPathwaySig}
#' @param covarcompare covariate comparison of interest
#' @param headerprefix columns of interest to be chosen from pathway results
#' @param fdrlevel level of false discovery rate to be considered
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #esPathwayTop(esPathwaySig_results, covarcompare, headerprefix = "ecu_pw_chi_sd_", fdrlevel = 0.05)
#' 
esPathwayTop <-
function(esPathwaySig_results, covarcompare, headerprefix = "ecu_pw_chi_sd_", fdrlevel=0.05) {
  #### Finds Top scoring Pathways (using Score Calculated by multiplying pathway eucledian distance and SD of Chi2
  cp2results = esPathwaySig_results
  covar = covarcompare
  
  pathway_data = data.frame(aggregate(cp2results,by=list(cp2results$pathway),FUN=mean,na.rm=TRUE))
  pathway_data$Genes = pathway_data$pathway = NULL
  names(pathway_data)[1] = "pathway"
  
  ### Setting up the variables
  pathway_score = paste0(headerprefix, covar[1], "_", covar[2])
  ecu = paste0("ecu_", covar[1], "_", covar[2])
  Chi_sd = paste0("Chi_sd_", covar[1], "_", covar[2])
  fdr = paste0(covar[1], "_", covar[2], ".FDR")
  columnoffdr = grep(fdr, labels(pathway_data)[[2]])   ## need this if column name starts with a number
  
  ### Ordering pathways by pathway score
  top_pathway = pathway_data[which(pathway_data[[columnoffdr]] < fdrlevel),]
  top_pathway = top_pathway[order(-top_pathway[[pathway_score]]),]
  
}
