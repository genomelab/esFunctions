#' esNMF
#' 
#' NMF for expressionset, returns a covariate vector. Non-Negative Matrix
#' Factorization (NMF) factorize a matrix V into two non-negative matrices WH.
#' 
#' @export
#' @param es expression set with all positive values
#' @param k is the rank, which is a specification of a single numeric value.
#' The the maximum linear independent columns of matrix A. You can determine
#' how many factorization you want.
#' @param run number of runs to perform
#' @param nmfmethod Default method "brunet"
#' @note Load package NMF.
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' 
#' \url{cran.r-project.org/web/packages/NMF/NMF.pdf}
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #NMF <- esNMF(Annoset, k=2, run = 100, nmfmethod = "brunet")
#' #NMF 
#' 
esNMF <-
function(es, k=2, run=100, nmfmethod="brunet"){
  ###########################
  ## NMF for expressionset, returns a covariate vector
  ###########################
  tt = Sys.time();
  nmfrun <- nmf(es, rank=k, nrun=run, method=nmfmethod, .opt='t')
  Sys.time() - tt
  name = paste(deparse(substitute(es)), ', method=', nmfmethod, ', runs=', run, ', k=', k, sep='')
  metaHeatmap(nmfrun, main=name)
  nmfcov = predict(nmfrun)
  return(list(nmfrun, nmfcov))
}
