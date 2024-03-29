#' esPlotpairs
#' 
#' Cuts out the underscore from esAnnotate function and plots it using ggpairs.
#' Converts to ggplot format for plotting and can also select genes and
#' covariates that are denoted in the argument.It also can reannotate an
#' expression set and modify the feature names that can be useful for plotting.
#' 
#' @export
#' @param es expression set object
#' @param select_covar list of covariate (varLabels of the expressionset)
#' @param features list of genes (featureNames of expressionset)
#' @param annot annotaiton file to use (for PSR or Transcripts)
#' @param annotate annotate (TRUE//FALSE) - Changes the featureNames to the
#' annotation needed
#' @param drop_endids drop_endids - In changing annotation, need to maintiain
#' uniqueness, which is done by joining the featureNames with the annotation
#' (using underscore), if TRUE for plotting purposes, the original featureNames
#' (ids) are not shown. %% ~~Describe \code{drop_endids} here~~
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #esPlotpairs(eset, select_covar="group_of_interest", features="genes_interest", annot="anno_data", annotate = TRUE, drop_endids = TRUE)
#' 
esPlotpairs <-
function(es, select_covar, features, annot, annotate = TRUE, drop_endids = TRUE) {
  ##################################################################
  ## Function 5
  ##  esPlotpairs(es, select_covar, features, annot, annotate = TRUE, drop_endids = TRUE)
  ##  Wrapper for ggpairs
  ##  es - expressionset class
  ##  select_covar - list of covariate (varLabels of the expressionset)
  ##  features - list of genes (featureNames of expressionset)
  ##  annot - annotaiton file to use (for PSR or Transcripts)
  ##  annotate (TRUE//FALSE) - Changes the featureNames to the annotation needed
  ##  drop_endids  - In changing annotation, need to maintiain uniqueness, which is done by 
  ##                 joining the featureNames with the annotation (using underscore), if TRUE
  ##                 for plotting purposes, the original featureNames (ids) are not shown.
  ##################################################################
  
  if (!annotate) {
    mod_features = features
    names = featureNames(es)  
  }
  else
  {
    ### Subset the expressionset based on the genes 
    ### annotate the expressionset first, find the genes, subset the expressionset
    ### then convert to ggplot format
    ##Reannotation and finding genes of interest
    es = esAnnotate(es, annot=annot)
    names = as.data.frame(matrix(unlist(strsplit(featureNames(es), split = "__")), ncol=2, byrow=TRUE))
    if (any(names$V1 %in% features)) {
      mod_features = paste(names$V1[names$V1 %in% features], '__', names$V2[names$V1 %in% features], sep="")
    }
    else
      if(any(names$V2 %in% features)) {
        mod_features = paste(names$V1[names$V2 %in% features], '__', names$V2[names$V2 %in% features], sep="")
      }
    else 
    {
      sprintf("No genes//probes found")
    }
  }
  
  #Subset the expressionset 
  es = es[featureNames(es) %in% mod_features,]
  
  #Conversion to ggplot format from expressionset and selecting genes and covariates
  es = es2df(es)
  selected = c(which(names(es) %in% select_covar), which(names(es) %in% mod_features))
  #selected = unique(unlist(lapply(select_cols, function(es) grep(es, names(gexprs)) )))   # Can be used if you want genes with similar patterns  (IL family)
  
  if (drop_endids)
  {
    colnames(es) = sub('(__[a-z0-9_-]*)','', colnames(es))
  }
  y = data.frame(es[,selected])
  
  ##### Different ways of displaying ggpairs:
  #ggpairs (y,  upper = list(continuous = "density", combo = "box"), missing=exclude)
  #, lower = list(continuous = "box", combo = "facehist"))  
  ggpairs(y, diag=list(continuous="density",   discrete="bar"), axisLabels="show")
}
