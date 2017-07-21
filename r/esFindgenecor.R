#' esFindgenecor
#' 
#' Finds nearest genes by variety of correlation types from a expressionset
#' class
#' 
#' @export
#' @param es expression set
#' @param features gene_symbols or other features of interest
#' @param numb_corr number of correlations to find
#' @param abscor Use absolute correlation
#' @param corrtype correlation type - 'spearman', 'pearson'
#' @param annotate annotate the data (TRUE//FALSE)
#' @param Anno annotation to use
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #results <- esFindgenecor(eset, features="features_of_interest", numb_corr = 50, abscor = TRUE, corrtype = "spearman", annotate = TRUE, Anno = annot)
#' #results
#' 
#' 
#' 
esFindgenecor <-
function(es, features, numb_corr = 50, abscor = TRUE, corrtype="spearman", annotate = TRUE, Anno=annot) {
  ##########################
  ## Finds nearest genes by variety of correlation types from a expressionset class
  ##  es - expression set
  ##  features - gene_symbols or other features of interest
  ##  numb_corr - number of correlations to find
  ##  abscor - Use absolute correlation
  ##  corrtype - correlation type - 'spearman', 'pearson'
  ##  annotate - annotate the data (TRUE//FALSE)
  ##  annot - annotaiton to use
  
  #3/1/14
  ## I changed the argument originally as Anno=annot, and used a single object annot because this object is passed into many different 
  #functions Esboxplot, esannotate, etc, so I just created a single object to be passed from each partition of the separate functions, 
  #instead of creating a second object anno=annot.  But I am not sure this is correct ; needs testing..
  ###########################
 annot<-NULL #must set the variable to NULL to pass R CMD check because it is not a global variable but passed down from a separate function
  
  
  selfeatures = esFeatures(es, features,  anno=Anno) #takes the anno=Anno and passes into esFeatures (which is a function of anno)
  location = (unlist(selfeatures[1]))
  
  if(annotate) {
    es = esAnnotate(es, annot=Anno) #takes Anno=annot and passes into esAnnotate;  
  }
  
  genecorr <- function(x) {
    correlation = cor(t(t(exprs(es)[x,])), t(exprs(es)), method=corrtype) 
    #correlation_pvalue = cor.test(t(t(exprs(es)[location[1],])), t(exprs(es)), method=type)
    if (abscor) {
      genecorr = correlation[,order(-abs(correlation))[1:numb_corr]]
    } else {
      genecorr = correlation[,order(-correlation)[1:numb_corr]]
    }
  }
  gene_cor = list(lapply(location, genecorr))
  
  gene_symbol_probe = names(gene_cor[[1]][[1]])
  gene_symbol = listToCharacterVector(lapply(gene_symbol_probe, function(x) strsplit(x, '__')[[1]][1]))
  gene_cor = gene_cor[[1]][[1]]
  results = data.frame(gene = rep(features, length(gene_cor)), gene_symbol, gene_symbol_probe, correlation = gene_cor)
  rownames(results) = c(1:length(gene_cor))
  return(results)
}
