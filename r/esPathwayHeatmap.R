esPathwayHeatmap <-
function(es, covar, covargroups , selpathway, pathwayanalysis, annotate=TRUE, col = colorRampPalette(c("navy", "white", "firebrick3"))(50), colorPalette = '') {
  
  if (is.null(covargroups)) {covargroups = c(levels(droplevels(factor(es[[covar]])))[1], levels(droplevels(factor(es[[covar]])))[2])}
  sp = unique(as.character(pathwayanalysis[grep(selpathway,pathwayanalysis$pathway),]$pathway))
  sgroup = paste0(covargroups[1], '_', covargroups[2])
  
  #return(print(cat("",pathsel)))
  
  for (i in c(1:length(sp)))  {  
    pathsel = as.character(sp[i])
    #return(print(cat("",pathsel)))
    genes = as.character(unlist(pathwayanalysis[pathwayanalysis$pathway == pathsel,]$genes))
    if (annotate) features = unlist(names(esFeatures(es, features=genes)[[1]])) else features= genes
    seles = es[features, which(es[[covar]] %in% covargroups)]
    esHeatmap(seles, covar=covar)
  }
}
