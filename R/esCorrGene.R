#' esCorrGene
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
#' #results <- esCorrGene(eset, features="features_of_interest", numb_corr = 50, abscor = TRUE, corrtype = "spearman", annotate = TRUE, Anno = annot)
#' #results
#' 
#' 
#' 
esCorrGene <-
function(es, features, numb_corr = 50, abscor = TRUE, corrtype="spearman", annotate = FALSE, anno = annot) {
    ##########################
    ## Finds nearest genes by variety of correlation types from a expressionset class
    ##  es - expression set
    ##  features - gene_symbols or other features of interest
    ##  numb_corr - number of correlations to find
    ##  abscor - Use absolute correlation
    ##  corrtype - correlation type - 'spearman', 'pearson'
    ##  annotate - annotate the data (TRUE//FALSE)
    ##  annot - annotaiton to use
    ###########################
    location = match(features, featureNames(es))
    if(annotate) {
      selfeatures = esFeatures(es, features, anno = anno)
      location = (unlist(selfeatures[1]))
      es = esAnnotate(es, anno=annot)
    }
    
    genecorr <- function(x) {
      correlation = cor(t(t(exprs(es)[x,])), t(exprs(es)), method=corrtype) 
      #correlation_pvalue = cor.test(t(t(exprs(es)[location[1],])), t(exprs(es)), method=type)
      if (abscor) {
        genecorr = correlation[,order(-abs(correlation))[1:numb_corr]]
      } else 
      {
        genecorr = correlation[,order(correlation)[1:numb_corr]]
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
