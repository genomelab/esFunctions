#' esPathwayplot
#' 
#' This will plot the pathway results based on chi-squared value and the
#' eucledian distance. Number of top pathways could be labeled with
#' \code{labeltopsigpaths} option.
#' 
#' 
#' @param esPathwaySig_results pathway analysis results from
#' \code{esPathwaySig}
#' @param covarcompare covariate comparison of interest
#' @param fdrlevel level of false discovery rate to be considered
#' @param labeltopsigpaths Number of top pathways could be labeled
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #esPathwayplot(esPathwaySig_results, covarcompare, fdrlevel = 0.05, labeltopsigpaths = c(1:5))
#' 
esPathwayplot <-
function(esPathwaySig_results, covarcompare, fdrlevel=0.05, labeltopsigpaths = c(1:5)) {
  #### Collapse the data to get pathway eucledian distance and SD of Chi2
  cp2results = esPathwaySig_results
  covar = covarcompare
  labeltop = labeltopsigpaths
  ######
  pathway_data = data.frame(aggregate(cp2results,by=list(cp2results$pathway),FUN=mean,na.rm=TRUE))
  pathway_data$Genes = pathway_data$pathway = NULL
  names(pathway_data)[1] = "pathway"
  
  ### Setting up the variables
  pathway_score = paste0("pathway_score_", covar[1], "_", covar[2])
  ecu = paste0("ecu_", covar[1], "_", covar[2])
  Chi_sd = paste0("Chi_sd_", covar[1], "_", covar[2])
  fdr = paste0(covar[1], "_", covar[2], ".FDR")
  columnoffdr = grep(fdr, labels(pathway_data)[[2]])   ## need this if column name starts with a number
  
  
  ### Create levels of fdr
  pathway_data$fdrlevel = rep(paste0("< ", fdrlevel), length(pathway_data[columnoffdr]))
  pathway_data$fdrlevel[which(pathway_data[[columnoffdr]] > fdrlevel)] = paste0("> ", fdrlevel)
  
  pathway_data$fdrsort = rep(0, length(pathway_data[columnoffdr]))
  pathway_data$fdrsort[which(pathway_data[[columnoffdr]] > fdrlevel)] = 1
  
  ### Ordering pathways by pathway score
  #top_pathway = pathway_data[order(-pathway_data[[pathway_score]]),][1:40]
  pathway_score = pathway_data[[pathway_score]]
  pathway_fdrsort = pathway_data$fdrsort
  fdrlevel = "fdrlevel"
  ###### Plot significance of pathways
  previous.theme = theme_set(theme_bw())
  p = ggplot(aes_string(x=ecu, y=Chi_sd, colour = fdrlevel), data = pathway_data) +
    xlab("Differntial Expression Pathway") +
    ylab("Variation of Significance of Genes in the Pathway (SD ChiSquare)") +
    geom_point(size=4)  +
    geom_text(data = pathway_data[order(pathway_fdrsort, -pathway_score),][labeltop,], 
              aes_string(x=ecu, y=Chi_sd, label="pathway"), 
              size = 4, hjust=0.6, vjust=-0.4, angle = 0) +
    theme(axis.title.x = element_text(size = 15, vjust = -1)) +
    theme(axis.title.y = element_text(size = 15, angle = 90, vjust = 0.25)) +
    theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 15)) +
    theme(panel.grid.minor = theme_blank()) +
    labs(title = paste0("Pathway Rank", covar[1], " versus ", covar[2])) +
    theme(legend.position = "none")
  p
}
