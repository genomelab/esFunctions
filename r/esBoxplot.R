#' esBoxplot
#' 
#' Box plot of selected features of an expressionset class, can be categorized
#' by a covariate
#' 
#' @export
#' @param es espression set
#' @param features feature names of the eset
#' @param covar covariate of eset
#' @param annotation Annotation data set that is used to convert the transcript
#' ID into the gene names
#' @param colPallette the name of the color pallette used
#' @param jitter logical command that will create a jitter plot in ggplot2
#' @param annotate logical command to include the annotation set
#' @param drop_ends logical command to drop the transcript ID from the gene
#' names
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #esBoxplot(es, features = "", covar = "", annotation = annot, colPallette = colorPalette("kmPalette2"), jitter = FALSE, annotate = FALSE, drop_ends = FALSE)
#' 
esBoxplot <-
function(es, features = '', covar='', annotation = annot, colPallette = colorPalette('kmPalette2'), jitter=FALSE, 
         annotate=FALSE, drop_ends=TRUE, ...){
  ##########################
  ## esBoxplot
  ## Box plot of selected features of an expressionset class, can be categorized by a covariate
  ###########################
  annot=NULL #this solves the note that comes from R CMD check, that says
  #the "no visible global function definition for annot"
  
  
  orderfeatures = features
  if (!annotate) {
    if(length(features)==0) {features = featureNames(es)}
    mod_features = features
    names = featureNames(es)  
  }  else
  {
    ### Subset the expressionset based on the genes or features
    ### annotate the expressionset first, find the genes, subset the expressionset
    ### then convert to ggplot format
    ##Reannotation and finding genes of interest
    es = esAnnotate(es, annot=annotation)
    mod_features = unlist((esFeatures(es, features,anno=annotation)[2]))
    
    #names = as.data.frame(matrix(unlist(strsplit(featureNames(es), split = "__")), ncol=2, byrow=TRUE))
    #if (any(names$V1 %in% features)) {
    #  mod_features = paste(names$V1[names$V1 %in% features], '__', names$V2[names$V1 %in% features], sep="")
    #} else
    #  if(any(names$V2 %in% features)) {
    #    mod_features = paste(names$V1[names$V2 %in% features], '__', names$V2[names$V2 %in% features], sep="")
    #  }
    #else 
    #{
    #  sprintf("No genes//probes found")
    #}
  }
  
  #Subset the expressionset 
  es = es[featureNames(es) %in% mod_features,]
  
  #Conversion to ggplot format from expressionset and selecting genes and covariates
  gges = es2df(es)
  
  if(nchar(covar)==0 || is.null(covar)) 
  {
    covar = 'no_selection'
    gges[[covar]] = c(rep('',dim(gges)[1]))
  }
  
  icovar = gges[[covar[1]]]
  if(length(covar)==2){
    icovar = interaction(gges[[covar[1]]], gges[[covar[2]]])
  }
  
  if(length(covar)==3){
    icovar = interaction(gges[[covar[1]]], gges[[covar[2]]])
    icovar = interaction(icovar, gges[[covar[[3]]]])
  }
  if(length(covar)>1) covar = paste(covar,collapse="_")
  
  gges[[covar]] = as.factor(icovar)
  
  #gges[[covar]] = factor(gges[[covar]])
  selected = c(which(names(gges) %in% covar), which(names(gges) %in% mod_features))
  #selected = unique(unlist(lapply(select_cols, function(x) grep(x, names(gexprs)) )))   # Can be used if you want genes with similar patterns  (IL family)
  
  gges = gges[,selected]
  gges = melt(gges, id.vars=covar)
  #print(head(gges))
  
  
  # Order genes based on the list provided
  gges$features =  sapply(as.character(gges$variable), function(x) substring(x,1,str_locate(x,'_')-1))[1,]
  gges$gene_order = match(gges$features, orderfeatures)   
  gges = gges[with(gges, order(gene_order, variable)),]
  gges$variable <- factor(gges$variable,
                          levels = unique(gges$variable))
  
  
  ### Change levels of selected covariate to include numbers in each item
  ### Change labeling of covar:
  ### Add the group numbers to the name
  tmpcovar = gges[[covar]]
  cov_levels = table(droplevels(as.factor(tmpcovar)))
  cov_levels_legend_heading = names(cov_levels)
  cov_levels_legend_numbers = cov_levels
  
  number_covar_levels = rep(1:length(cov_levels_legend_heading))
  for(x in number_covar_levels) { 
    levels(tmpcovar)[levels(tmpcovar)==cov_levels_legend_heading[x]] <- 
      paste0(cov_levels_legend_heading[x], "  n=" , cov_levels_legend_numbers[x]/length(levels(gges$variable)))
  }
  
  gges[[covar]] = tmpcovar
  
  
  if (!annotate) {
    gges$features =  gges$variable
    gges$gene_order = match(gges$features, orderfeatures)   
    gges = gges[with(gges, order(gene_order, variable)),]
    gges$variable <- factor(gges$variable,
                            levels = unique(gges$variable))  }
  
  
  if (drop_ends)
  {
    
    variable_order = sapply(levels(gges$variable), function(x) substring(x,1,str_locate(x,'_')-1))[1,]
    previous.theme = theme_set(theme_bw()) #set black and white ggplot theme
    variable = 'variable'
    value = 'value'
    
    p = ggplot(aes_string(x=variable, y=value, fill=covar), data=gges) + 
      geom_boxplot() + 
      xlab("Genes") + 
      ylab("Expression Value") +
      position = position_dodge(width = 1), 
      width = 0.5,
      #ylim(0, max(gges$value))+
      theme(axis.title.x = element_text(face="bold",  vjust=-.3, size=20), 
           axis.title.y = element_text(face="bold", angle=90, size=20),
           axis.text.x  = element_text(angle=90, size=16),
           axis.text.y = element_text(size=16)) +
      scale_x_discrete(breaks=names(variable_order), 
                       labels=variable_order) +
      scale_fill_manual(values=colPallette) 
    if (jitter) {p = p + geom_jitter(); print(p)} 
    else {print(p)}
  }
  else
  {
    previous.theme = theme_set(theme_bw()) #set black and white ggplot theme
    variable2 = 'variable2'
    variable = 'variable'
    value = 'value'
    
    if(covar == 'no_selection')
    {
      theme_set(theme_grey())
      p = ggplot(aes_string(x=variable, y=value), data=gges) +     
        geom_boxplot(position=position_dodge(width=.8)) + 
        xlab("Genes") + 
        ylab("Expression Value") +
        position = position_dodge(width = 1), 
        width = 0.5,
        #ylim(0, max(gges$value))+
        theme(axis.title.x = element_text(face="bold",  vjust=-.6, size=20), 
             axis.title.y = element_text(face="bold", angle=90, size=20),
             axis.text.x  = element_text(angle=90, size=16),
             axis.text.y = element_text(size=16)) 
    }
    else
    {
      
      p = ggplot(aes_string(x=variable, y=value), data=gges) +     
        geom_boxplot(aes_string(fill=covar), position=position_dodge(width=.8)) + 
        xlab("Genes") + 
        ylab("Expression Value") +
        position = position_dodge(width = 1), 
        width = 0.5,
        #ylim(0, max(gges$value))+
        theme(axis.title.x = element_text(face="bold",  vjust=-.6, size=20), 
             axis.title.y = element_text(face="bold", angle=90, size=20),
             axis.text.x  = element_text(angle=90, size=16),
             axis.text.y = element_text(size=16)) +
        scale_fill_manual(values=colPallette)
    }
    
    
    
    #if (jitter) {p = p + geom_jitter(position=position_jitter(width=0, height=1)); print(p)} 
    #else {print(p)}
    print(p)
  }    
  p
}
