#' esHeapmap
#' 
#' Takes the features of an expression set and create an annotated heat map
#' where the rows are the features of the expression set and the columns are
#' the covariate.
#' 
#' 
#' @param es expression set
#' @param covar ccovariate
#' @param rownames Default setting is NULL. Give every feature names. If it is
#' not set to NULL, the function will label the row names given into the
#' function and match the feature names.
#' @param hmapcol Colors based on the heat map. If is specificed as NULL, there
#' is a default color setting.
#' @param colorPalette colorPallette has to be a list of vectors, each with
#' number of colors. By default it creates a recurring list of colors obtained
#' from global kmPallette, Pallette1, Pallette2 covar should be a list of
#' characters i.e. c('risk', 'efscens).
#' @param distfun Default parameter to complete linkage clustering using
#' euclidean distance measure and you can control this by specifying it by
#' different functions to hclustFun or disfun, for example Manhattan distance.
#' @param hclustfun function used to compute the hierarchial clustering when
#' Rowv or Colv are not dendrograms
#' @param scale where the data points are centered
#' @param featurelimit limits you can have
#' @param cexRow Row axis labeling feature. Specify the tick or text label with
#' a numeric value
#' @param ...  This is a placeholder
#' @note Package NMF must be installed first
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' 
#' http://rss.acs.unt.edu/Rdoc/library/Heatplus/html/heatmap_plus.html
#' 
#' http://nmf.r-forge.r-project.org/aheatmap.html
#' @examples
#' 
#' 
#' #esHeatmap(eset, covar="covariate_of_interest")
#' 
esHeatmap = function(es, covar, rownames = NULL , hmapcol = NULL, colorPalette = '', 
                     distfun = "euclidean", hclustfun = "average", scale="none", 
                     featurelimit=5000, cexRow = 0.05 +  1/log10(dim(exprs(es))[1]), ...) {
  
  #3/1/14 -- made a change in line line 35 to color=hmapcol ;  originally it was col = hmapcol; this gave errors to R CMD check
  
  ### colorPallette has to be a list of vectors, each with number of colors
  ### By default it creates a recurring list of colors obtained from global kmPallette, Pallette1, Pallette2
  ### covar should be a list of characters i.e. c('risk', 'efscens)
  if (is.null(hmapcol)) hmapcol = colorRampPalette(c("darkgreen", "yellowgreen", "lightgoldenrodyellow", "darkorange", "firebrick3"))(50)
  #colorRampPalette(c("navy", "white", "firebrick3"))(50)
  if (colorPalette=='') {
    colorPalette = rep(list(colorPalette('Pallette4'), colorPalette('Pallette5'), colorPalette('Palette1'), colorPalette('Palette2'), colorPalette('Pallette3'), length(covar)))
  }
  ann_Col = list()
  ann_colors = list()
  counter = 1
  for (var in covar) {
    varcolors = colorPalette[[counter]][1:length(levels(factor(es[[var]])))]
    names(varcolors) = levels(factor(es[[var]]))
    ann_colors[counter] = list(varcolors) 
    
    ann_Col[counter] = list(as.character(es[[var]]))
    
    counter = counter + 1
  }
  names(ann_colors) = covar
  names(ann_Col) = covar
  
  
  if (is.null(rownames)) lab_row = featureNames(es) else {lab_row = rownames; es=es[rownames,]}
  if (length(featureNames(es))  > featurelimit)  return(sprintf(paste0("Greater than ", featurelimit, " features, change limit variable in the function.")))
  #exprs(es) = 2^exprs(es)                                       
  p = aheatmap (es, 
                color=hmapcol,
                scale= scale,
                distfun = distfun,
                hclustfun = hclustfun,
                legend = TRUE,
                annCol = ann_Col,
                annColors = ann_colors,
                fontsize = 16,
                cexRow = cexRow ,
                cexCol = 0.2 + 1/log10((dim(exprs(es))[2])), ...
                #labRow = NULL
  )
}
