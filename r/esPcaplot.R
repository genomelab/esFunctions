#' esPcaplot
#' 
#' esPCA plot of selected features of an expressionset class
#' 
#' @export
#' @param es expression set
#' @param features features = set of featurenames of the expression set, if
#' left blank all are used
#' @param covar used to color the points in the PCA
#' @param labelvar used to label the points in the PCA
#' @param pcs used to state which principle components to plot
#' @param shape TRUE/FALSE. TRUE if you want to add shapes.
#' @param kmcolors Loads the color palette.
#' @param scale Logical command that scales the data. TRUE for scale.
#' @param center Logical command that centers the data. TRUE for center.
#' @note For argument kmcolors, one can create their own color palette called
#' kmPalette <- c("red", "blue", "darkgreen", "black", "orange, "yellow4",
#' "brown", "purple", "grey", "turquoise"). The amount of colors must match the
#' number of levels.
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' # kmcolor, look at the note
#' #esPcaplot(eset, covar="group_of_interest", shape = FALSE, kmcolors = colorPalette("kmPalette"))
#' 
esPcaplot = function(es, features = '', covar='', labelvar='', pcs = c(1,2), shape=FALSE, kmcolors = colorPalette('kmPalette'), scale=TRUE, center=TRUE) {
  ##########################
  ## Function 23
  ## esPcaplot
  ## PCA plot of selected features of an expressionset class
  ##  features = set of featurenames of the expression set, if left blank all are used
  ##  covar = used to color the points in the PCA
  ##  labelvar = used to label the points in the PCA
  ##  pcs = used to state which principle components to plot
  ###########################
  
  if(typeof(es) == 'list')  #### A dataframe passed to it, let's make it into a expression set and then perform PCA
  {
    es = df2es(es)
  }
  
  if(length(features)==1) {features = featureNames(es)}
  es = es[features,]
  
  target.PC = prcomp(t(exprs(es)), scale=scale, center=center)
  target.scores = predict(target.PC)
  eigs <- target.PC$sdev^2
  PC1var  = eigs[1] / sum(eigs)
  PC2var  = eigs[2] / sum(eigs)
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
  
  targetPCA[[labelvar]] = as.factor(targetPCA[[labelvar]])
  #targetPCA[[covar]] = as.factor(targetPCA[[covar]])
  #sprintf(paste('targetPCA is ', typeof(targetPCA)))
  #typeof(targetPCA)
  #attach(target.PCA)
  
  PCx = paste("PC", pcs[1], " (Variance ", PC1var, "%", sep='')
  PCy = paste("PC", pcs[2], " (Variance ", PC2var, "%", sep='')
  
  ### Draw the PC1 and PC2 plot
  previous.theme = theme_set(theme_bw()) #set black and white ggplot theme
  
  if(!shape){
    p = ggplot(aes_string(x=PCx, y=PCy), data=targetPCA) + 
      labs(x = PCx) + 
      labs(y = PCy) + 
      labs(title = '', size = 18, legend.position = "right") +
      scale_colour_manual(values=kmcolors) +
      geom_point(color="grey50", size = 5) +
      geom_point(aes_string(color = covar), data=targetPCA, size=4) +
      geom_text(aes_string(x=PCx, y=PCy, label=labelvar), data=targetPCA, size = 4, hjust=0.6, vjust=-0.7, angle = 0) +  
      theme(axis.title.x = element_text(size = 25, vjust = -1)) +
      theme(axis.title.y = element_text(size = 25, angle = 90, vjust = 0.25)) +  
      theme(axis.text.x = element_text(size = 24)) +
      theme(axis.text.y = element_text(size = 24)) +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "PCA Analyses")
    print (p)
  }
  else
  {
    
    p = ggplot(aes_string(x=PCx, y=PCy), data=targetPCA) +
      labs(x = PCx) + 
      labs(y = PCy) + 
      labs(title = '', size = 18, legend.position = "right") +
      scale_colour_manual(values=kmcolors) +
      #geom_point(color="black", size = 5.5) +
      geom_point(aes_string(shape = labelvar), data=targetPCA, color='black', size=5.5) +
      geom_point(aes_string(shape = labelvar), data=targetPCA, color="grey50", size = 4) +
      geom_point(aes_string(shape = labelvar), data=targetPCA, color="white", size = 3.7) +
      geom_point(aes_string(color = covar), data=targetPCA, size = 3) +
      geom_text(aes_string(x=PCx, y=PCy, label=labelvar), data=targetPCA, size = 4, hjust=0.6, vjust=-0.7, angle = 0) +  
      theme(axis.title.x = element_text(size = 25, vjust = -1)) +
      theme(axis.title.y = element_text(size = 25, angle = 90, vjust = 0.25)) +  
      theme(axis.text.x = element_text(size = 24)) +
      theme(axis.text.y = element_text(size = 24)) +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "PCA Analyses")
    print (p)
  }
  p
}
