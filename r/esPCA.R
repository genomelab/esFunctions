#' Principal Component Analysis
#' 
#' Principal Component Analysis uses eigenvectors and eigenvalues to create a
#' linear subspace that span the data points along the direction of maximum
#' variance.  Usually the first three principal components are considered only.
#' 
#' It column binds the rows of the assay data matrix of the expression set ,
#' into a new expression set.  It is usually helpful to then plot the PC1 and
#' PC2 on a new plane for analysis.
#' 
#' @export
#' @param es es is the Expression Set that is coupled with the covariate
#' matrix.
#' @param features The features are the gene names or transcript ID's , or the
#' rows of the assay data matrix used in the Expression set.  For best use,
#' features is usually left blank to evaluate all of them.
#' @return There are no values returned in this function, only a new expression
#' set with principal components binded to the columns %% ~Describe the value
#' returned %% If it is a LIST, use %% \item{comp1 }{Description of 'comp1'} %%
#' \item{comp2 }{Description of 'comp2'} %% ...
#' @note In the below example, we leave out features so that all the rows are
#' evaluated.  To customize selected features enter the feature names under the
#' second argument 'features'.  Note: for the second example below, there are
#' three feature names arbitrarily selected.  The output for neweset2 will show
#' only three principal components that are related only to these customized
#' features that were selected.
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1
#' @examples
#' 
#' # See the references for creating an expression set.  Assuming one is made, adding the principal components is straight forward.
#' #neweset<- esPCA(oldeset)
#' #View(neweset) 
#' 
#' # Second Example with Customized Features used in PCA
#' #neweset2<-esPCA(oldeset, c('featurename1', 'featurename2', 'featurename3'))
#' #View(neweset2)
#' 
esPCA <-
function(es, features = '')   {
  ##########################
  ## Function 22
  ##   generates a dataframe of principle components and addes it to es
  ##   returns es
  ############################
  
  if(length(features)==1) {features = featureNames(es)}
  es = es[features,]
  target.PC = prcomp(t(exprs(es)), scale=TRUE, center=TRUE)
  target.scores = predict(target.PC)
  pheno = cbind(pData(es), sample = rownames(pData(es)))
  phenoPCA = merge(target.scores,pheno,by.x = 0,by.y = 0)
  rownames(phenoPCA) = phenoPCA[[1]]
  phenoPCA[1] = NULL
  phenoData <- new("AnnotatedDataFrame", data=phenoPCA)
  es <- new("ExpressionSet", exprs=exprs(es), phenoData=phenoData)
}
