#' esPcaplot3d
#' 
#' esPcaplot3d, works on macs or systems with X windows. PCA plot of selected
#' features of an expressionset class in 3D.
#' 
#' 
#' @param es es = eset name
#' @param features features = set of featurenames of the expression set, if
#' left blank all are used
#' @param covar covar = used to color the points in the PCA
#' @param labelvar labelvar = used to label the points in the PCA
#' @param pcs pcs = used to state which principle components to plot
#' @param scale scale = scale the pca
#' @param center center = center the pca
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' # Requires package RGL
#' #esPcaplot3d(eset, covar="covariate_of_interest")
#' 
esPcaplot3d <-
function(es, features = '', covar='', labelvar='ignored', pcs = c(1,2,3), scale=TRUE, center=TRUE) {
  ##########################
  ## Function 24 
  ## esPcaplot3d, works on macs or systems with X windows
  ## PCA plot of selected features of an expressionset class in 3D
  ##  features = set of featurenames of the expression set, if left blank all are used
  ##  covar = used to color the points in the PCA
  ##  labelvar = used to label the points in the PCA
  ##  pcs = used to state which principle components to plot
  ###########################
  
  plot3d<- NULL # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable.  
  
  
  if(length(features)==1) {features = featureNames(es)}
  es = es[features,]
  
  target.PC = prcomp(t(exprs(es)), scale=scale, center=center)
  target.scores = predict(target.PC)
  pheno = cbind(pData(es), sample = rownames(pData(es)))
  targetPCA = merge(target.scores,pheno,by.x = 0,by.y = 0)
  rownames(targetPCA) = targetPCA[[1]]
  targetPCA[1] = NULL
  
  if(nchar(labelvar)==0) 
  {
    labelvar = 'empty'
    targetPCA[[labelvar]] = c(rep('',dim(targetPCA)[1]))
  }
  
  if(nchar(covar)==0) 
  {
    covar = 'no_selection'
    targetPCA[[covar]] = c(rep('',dim(targetPCA)[1]))
  }
  
  icovar = targetPCA[[covar[1]]]
  if(length(covar)==2){
    icovar = interaction(targetPCA[[covar[1]]], targetPCA[[covar[2]]])
  }
  
  if(length(covar)==3){
    icovar = interaction(targetPCA[[covar[1]]], targetPCA[[covar[2]]])
    icovar = interaction(icovar, targetPCA[[covar[[3]]]])
  }
  covar = paste(covar,collapse="_")
  targetPCA[[covar]] = as.factor(icovar)
  
  #targetPCA[[covar]] = as.factor(targetPCA[[covar]])
  #sprintf(paste('targetPCA is ', typeof(targetPCA)))
  #typeof(targetPCA)
  #attach(target.PCA)
  
  PCx = targetPCA[[paste("PC", pcs[1], sep='')]]
  PCy = targetPCA[[paste("PC", pcs[2], sep='')]]
  PCz = targetPCA[[paste("PC", pcs[3], sep='')]]
  covar = targetPCA[[covar]]
  
  plot3d(PCx, PCy, PCz, col=covar, type = "s", size=1, box=F)
}
