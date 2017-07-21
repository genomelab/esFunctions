#' esSil
#' 
#' Identify and draw sil_width for a clustering pattern. It requires package
#' 'cluster'. It adds the NMF cluster and silhouette width in the es dataframe
#' (using covar_name + cluster or sil_width)
#' 
#' 
#' @param es expression set
#' @param clusters Number of clusters
#' @param covar_name covariate name
#' @note Requires package 'cluster'
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #esSil(eset, clusters, covar_name = "covariate_name_of_interest")
#' 
esSil <-
function(es, clusters, covar_name=''){
  ###########################
  ## Identify and draw sil_width for a clustering pattern
  ## requires package 'cluster'
  ## It adds the NMF cluster and silhoette width in the es dataframe (using covar_name + cluster or sil_width)
  ###########################
  dissE = daisy(t(exprs(es)))
  dissEsqr = dissE^2
  sk = silhouette(as.integer(clusters), dissE)
  #plot(sk)
  sk2 = silhouette(as.integer(clusters), dissEsqr)
  #plot(sk2)
  plot(sk2, col = c("blue"))
  #sil = list(sk[,3], sk2[,3])
  name <- as.character(all.vars(substitute(clusters))) 
  if (covar_name=='') 
  {
    colnames(sk2) = lapply(colnames(sk2), function(x) paste0(name,'_',x)) }
  else
  {
    colnames(sk2) = lapply(colnames(sk2), function(x) paste0(covar_name,'_',x))  
  }
  out = data.frame(sk2[,c(1)], sk2[,c(3)]) 
  rownames(out) = sampleNames(es)
  colnames(out) = c(colnames(sk2)[1], colnames(sk2)[3])
  out
}
