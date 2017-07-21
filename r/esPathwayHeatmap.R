#' esPathwayHeatmap
#' 
#' Plot a heat map for genes belong to a specific pathway using the expression
#' set and the pathway analysis results.
#' 
#' 
#' @param es expression set
#' @param covar covariate
#' @param covargroups covariate groups of interest
#' @param selpathway select pathway of interest
#' @param pathwayanalysis pathway analysis results
#' @param annotate annotation. \code{TRUE} or \code{FALSE}
#' @param col color palette
#' @param colorPalette colorPallette has to be a list of vectors, each with
#' number of colors. By default it creates a recurring list of colors obtained
#' from global kmPallette, Pallette1, Pallette2 covar should be a list of
#' characters i.e. c('risk', 'efscens).
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #esPathwayHeatmap(eset, covar="covariate_of_interest", covargroups, selpathway, pathwayanalysis, annotate = TRUE, col = colorRampPalette(c("navy", "white", "firebrick3"))(50))
#' 
esPathwayHeatmap <-
function(es, covar, covargroups , selpathway, pathwayanalysis, annotate=TRUE, col = colorRampPalette(c("navy", "white", "firebrick3"))(50), colorPalette = '') {
  
  if (is.null(covargroups)) {covargroups = c(levels(droplevels(factor(es[[covar]])))[1], levels(droplevels(factor(es[[covar]])))[2])}
  sp = unique(as.character(pathwayanalysis[grep(selpathway,pathwayanalysis$pathway),]$pathway))
  sgroup = paste0(covargroups[1], '_', covargroups[2])
  
  #return(print(cat("",pathsel)))
  
  for (i in c(1:length(sp)))  {  
    pathsel = as.character(sp[i])
    #return(print(cat("",pathsel)))
    genes = as.character(unlist(pathwayanalysis[pathwayanalysis$pathway == pathsel,]$genes))
    if (annotate) features = unlist(names(esFeatures(es, features=genes)[[1]])) else features= genes
    seles = es[features, which(es[[covar]] %in% covargroups)]
    esHeatmap(seles, covar=covar)
  }
}
