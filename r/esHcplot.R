esHcplot <-
function(es, features, selcovar, base, winsor=TRUE, title="", scale=FALSE) {
  
  #########################################################################
  ##  Function 6
  ##  Generic Heirarchical Clustering using data that are Windsorized
  ##  
  ##  Need to set winsor levels in the function (levels set at 10% and 90%)
  ##  es - expressionset
  ##  selcovar = groups that will be used to determine how to calculate the fold change
  ##  base = the mean level of features from this selcovar group will be used as base of comparison 
  ##          for all samples. 
  ##  scale - If you do not winsorize then set scale to True to get regular heatmap 
  ##########################################################################
  
  #3/1/14 --  I changed the heatmap.2 and heatmap.plus inner parameters to distfun=dist2.  this
  #must match the distfun argument in the heatmap functions; R CMD check requires this.
  
  # Select subset of samples 
  if (length(features)  > 5000) {
    return(print("Greater than 5000 features, decrease the number."))
  }
  
  ### If using Winsor (default), color levels need to be low and don't need to scale values (normalize with SD 1)
  ### If scale is said to be true, then we unlog the values and let heatmap standardize the values and need to pick large number of color levels.
  
  scalevalue = "none"
  colorlevel = 20
  if (scale) {
    scalevalue = "row"
    colorlevel = 80  
  }
  
  
  ############# Generate Heatmap 
  #Generate colors for various covariates
  ### Setting up colors for the Heatmap
  ### Defining Colors:
  #colors()[grep("red",colors())]  #List of Colors, can change to red//oragne or whatever to get a list to use.
  cmap21 = c(GetColorHex('red'), GetColorHex('blue'))
  cmap22 = c(GetColorHex('ivory'), GetColorHex('black'))
  cmap23 = c(GetColorHex('blue'), GetColorHex('black'))
  cmap31 = c(GetColorHex('green'), GetColorHex('red'), GetColorHex('blue'))
  cmap32 = c(GetColorHex('rosybrown'), GetColorHex('orange'), GetColorHex('darkturquoise'))
  cmap41 = c(GetColorHex('green'), GetColorHex('blue'), GetColorHex('red'), GetColorHex('black'))
  
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
  
  s.color = matrix(as.character(c(
    sapply(as.numeric(factor(selcovar)),colpick41),
    sapply(as.numeric(factor(selcovar)),colpick41),
    sapply(as.numeric(factor(selcovar)),colpick41), 
    sapply(as.numeric(factor(selcovar)),colpick41))), 
                   nrow=length(selcovar), ncol=4)
  colnames(s.color) = c('', '', 'Disease', '')
  print(levels(selcovar))
  print(cmap31)
  
  
  
  ### Setup for clustering type and distance function for heatmap
  hclust2 <- function(x, method="average", ...)
    hclust(x, method=method, ...)
  dist2 <- function(x, ...)
    as.dist(1-abs(cor(t(x), method="pearson")))
  #dist2 <- function(x, ...)
  #  dist(x,method="euclidean")
  
  
  
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
                    main = title, labCol = NA, labRow = featureNames(es), keep.dendro=FALSE, cexRow = 0.05 +  1/log10(dim(exprs(es))[1]), 
                    cexCol = 0.2 + 1/log10((dim(exprs(es))[2])))         
  
  #under the heatmap.plus function for the input labRow, it was initially labeled as labRow=genenames, however
  #when running R CMD check, it could not find genenames, so I input featureNames(es)
  #which will print all of them.
  
  
  #dev.off()
  
  #cutree()
  
  return(sprintf('%s samples and %s features in %s group', dim(exprs(es))[2] , dim(exprs(es))[1], levels(selcovar)))
  
}
