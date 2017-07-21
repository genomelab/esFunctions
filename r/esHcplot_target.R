#' Heirarchical Clustering
#' 
#' Heirarchical Clustering using data that are Windsorized. Generates HC and
#' map of HC and can have columns representing phenotypes on top row,
#' specifically written for TARGET data.
#' 
#' 
#' @param es expression set
#' @param features the rows of the expression set
#' @param selcovar select covariate group of interest that you want label on
#' the cluster
#' @param winsor winsorize set to TRUE. It will fix the outliers.
#' @param title Main title of the plot
#' @param scale If scale is said to be true, then we unlog the values and let
#' heatmap standardize the values and need to pick large number of color
#' levels.
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #esHcplot_target(eset, features= c("feature1"), selcovar="group_of_interest", winsor = TRUE, title = "Hc Plot", scale = FALSE)
#' 
esHcplot_target <-
function(es, features, selcovar, winsor=TRUE, title="", scale=FALSE) {
  #########################################################################
  ##  Heirarchical Clustering using data that are Windsorized 
  ##
  ##  Generates HC and map of HC and can have columns representing
  ##  phenotypes on top row, specifically written for TARGET data
  ##########################################################################
  
  #3/1/14 --  I changed the heatmap.2 and heatmap.plus inner parameters to distfun=dist2.  this
  #must match the distfun argument in the heatmap functions; R CMD check requires this.
  
  # Select subset of samples 
  if (length(features)  > 5000) {
    return(sprintf("Greater than 5000 features, decrease the number."))
  }
  
  ## This means a data.table (data.frame) was sent in with column 1 being porbe_id and column2 being gene name
  # if (dim(features)[2] > 1) { 
  #     probes = features[[1]]
  #     genenames = features[[2]] 
  # }
  # if (dim(features)[2] == 1) {
  #     probes = features[[1]]
  #     genenames = features[[1]]   
  # }
  # 
  # probes = unique(probes[probes %in% featureNames(exprset)])
  # es = exprset[as.character(probes),]
  # genenames = features[features[[1]] %in% featureNames(es)][[2]]   
  genenames = features
  probes = features
  
  if (selcovar == "NMF"){
    #selectstatement<-"(es$risk=='high' | es$risk == 'low')"
    #iselect<-eval(parse(text=selectstatement))
    #es = es[, which(iselect==TRUE)]
    #es = es[, order(-as.numeric(es$NMF.HRA), es$mycn, es$mycn)]
    lowerrisk = exprs(es[, which(es$NMF.HRA=="HRN")])
  }
  
  if (selcovar == "ALL"){
    #selectstatement<-"(es$risk=='high' | es$risk == 'low')"
    #iselect<-eval(parse(text=selectstatement))
    #es = es[, which(iselect==TRUE)]
    es = es[, order(-as.numeric(es$risk), es$mycn, es$mycn)]
    lowerrisk = exprs(es[, which(es$risk=="low")])
  }
  
  if (selcovar == "HRAvsLR"){
    selectstatement<-"(es$risk=='HRA' | es$risk == 'LR')"
    iselect<-eval(parse(text=selectstatement))
    es = es[, which(iselect==TRUE)]
    es = es[, order(-as.numeric(es$risk), es$efscens)]
    
    lowerrisk = exprs(es[, which(es$risk=="LR")])
    higherrisk = exprs(es[, which(es$risk=="HRA")])
  }
  
  if (selcovar == "HRNvsLR"){
    selectstatement<-"(es$risk=='HRN' | es$risk == 'LR')"
    iselect<-eval(parse(text=selectstatement))
    es = es[, which(iselect==TRUE)]  
    es = es[, order(-as.numeric(es$risk), es$efscens)]
    
    
    
    lowerrisk = exprs(es[, which(es$risk=="LR")])
    higherrisk = exprs(es[, which(es$risk=="HRN")])
  }
  
  if (selcovar == "HRAvsHRN"){  
    selectstatement<-"(es$risk=='HRA' | es$risk == 'HRN')"
    iselect<-eval(parse(text=selectstatement))
    es = es[, which(iselect==TRUE)]
    # Following line orderse the expressionset for displaying in heatmap
    es = es[, order(-as.numeric(es$risk), es$efscens)]
    
    #Following lines are used later on to find the average in low risk group, subtracting
    #from everything and then windsorize it 10, 90%
    lowerrisk = exprs(es[, which(es$risk=="HRN")])
    higherrisk = exprs(es[, which(es$risk=="HRA")])
  }
  
  
  ### If using Winsor (default), color levels need to be low and don't need to scale values (normalize with SD 1)
  ### If scale is said to be true, then we unlog the values and let heatmap standardize the values and need to pick large number of color levels.
  
  scalevalue = "none"
  colorlevel = 20
  if (scale) {
    scalevalue = "row"
    colorlevel = 80  
  }
  
  
  
  if (winsor) {
    #Normalize to a calibrator delta-deltaCT (in our case we'll create a fake sample made up of average of pts < 18 months in CCG)
    row.means.younger <- apply(lowerrisk, 1, mean)
    #row.means.younger <- apply(t(data), 1, mean)
    exprs(es) = 2^(exprs(es) - row.means.younger)
    data = exprs(es)
    upper = quantile(data, .90)
    lower = quantile(data, 0.1)
    #winsbyrow = function(x) {
    #  pmax(pmin(x, quantile(x, .9)), quantile(x, .1))
    #}
    
    winsoverall = function(x) {
      pmax(pmin(x, upper), lower)
    }
    data = apply(data, 2, winsoverall)
    exprs(es) = data
  }
  
  ######## Generate Heatmap 
  #Generate colors for various covariates
  ### Setting up colors for the Heatmap
  ### Defining Colors:
  #colors()[grep("red",colors())]  #List of Colors, can change to red//oragne or whatever to get a list to use.
  cmap21 = c(GetColorHex('red'), GetColorHex('blue'))
  cmap22 = c(GetColorHex('ivory'), GetColorHex('black'))
  cmap23 = c(GetColorHex('blue'), GetColorHex('black'))
  cmap31 = c(GetColorHex('red'), GetColorHex('black'), GetColorHex('blue'))
  cmap32 = c(GetColorHex('rosybrown'), GetColorHex('orange'), GetColorHex('darkturquoise'))
  cmap41 = c(GetColorHex('red'), GetColorHex('blue'), GetColorHex('white'), GetColorHex('black'))
  
  #colorset = data.frame(c(cmap21,cmap31,cmap32))   #needs fixing later to create a color data frame for later use
  colpick21 = function(x)
  {  x = cmap21[x] }
  colpick22 = function(x)
  {  x = cmap22[x] }
  colpick23 = function(x)
  {  x = cmap23[x] }
  colpick31 = function(x)
  {  x = cmap31[x] }
  colpick41 = function(x)
  {  x = cmap41[x] }
  ### Setup the covariate as particular color combinations for display above the heatmap, here we will display
  ### three covariates.
  
  
  #   s.color = matrix(as.character(c(
  #     sapply(as.numeric(factor(es$mycnrisk)),colpick31), 
  #     sapply(as.numeric(factor(es$mycnrisk)),colpick31), 
  #     sapply(as.numeric(factor(es$efscens)),colpick22), 
  #     sapply(as.numeric(factor(es$scens)),colpick22))), 
  #                    nrow=length(es$risk), ncol=4)
  #   colnames(s.color) = c('Risk', '', 'Survival', '')
  #   
  #   s.color = matrix(as.character(c(
  #     sapply(as.numeric(factor(es$mycnrisk)),colpick41), 
  #     sapply(as.numeric(factor(es$mycnrisk)),colpick41), 
  #     sapply(as.numeric(factor(es$mycnrisk)),colpick41), 
  #     sapply(as.numeric(factor(es$mycnrisk)),colpick41))), 
  #                    nrow=length(es$risk), ncol=4)
  #   colnames(s.color) = c('', '', 'Risk', '')
  #   
  s.color = matrix(as.character(c(
    sapply(as.numeric(factor(es$NMF.HRA)),colpick41), 
    sapply(as.numeric(factor(es$NMF.HRA)),colpick41), 
    sapply(as.numeric(factor(es$NMF.HRA)),colpick41), 
    sapply(as.numeric(factor(es$NMF.HRA)),colpick41))), 
                   nrow=length(es$risk), ncol=4)
  colnames(s.color) = c('', '', 'Risk', '')
  
  
  ### Setup for clustering type and distance function for heatmap
  hclust2 <- function(x, method="average", ...)
    hclust(x, method=method, ...)
  dist2 <- function(x, ...)
    as.dist(1-abs(cor(t(x), method="pearson")))
  dist2 <- function(x, ...)
    dist(x,method="euclidean")
  
  
  
  ### use heatmap.2 to draw the key (can't display multiple covariates on top of heatmap)
  par(cex.axis=1, cex.sub=1, cex.main = 1, font.axis=1)
  heatmap.2 (exprs(es), col=jetColors(colorlevel), scale=scalevalue,  margins = c(5, 1), dendrogram = "none",
             ColSideColors=s.color[,1], distfun = dist2, hclustfun = hclust2, density.info = "none", 
             trace="none", key = T, Rowv=NA, Colv=NA, labCol = NA, cexRow = .9, cexCol = 0.9,
             lmat=rbind( c(0, 3), c(2,1), c(0,4) ), lhei=c(1.5, 4, 2 ) 
  )
  
  ### use heatmap.plus, can draw multiple covariates on top (but can't make a key!)
  #postscript("heatmap.ps", height=4.75, width=10)
  #compression = "none")
  par(cex.axis=1, cex.main = 1, font.axis=1)
  p = heatmap.plus (exprs(es), col=jetColors(colorlevel), scale=scalevalue, margins = c(1,15),
                    ColSideColors=s.color[,1:4], distfun = dist2, hclustfun = hclust2, Rowv=TRUE, Colv=TRUE, 
                    main = title, labCol = NA, labRow = genenames, keep.dendro=FALSE, cexRow = 0.05 +  1/log10(dim(exprs(es))[1]), 
                    cexCol = 0.2 + 1/log10((dim(exprs(es))[2])))         
  #dev.off()
  #cutree()  
  return(sprintf('%s samples and %s features in %s group', dim(exprs(es))[2] , dim(exprs(es))[1], selcovar))
}
