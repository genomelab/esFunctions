#' esFilter
#' 
#' Filter function to filter expression set class. Based on cv of each
#' expression (greater or less than) certain cv limit and percent of samples
#' with (greater or less than) certain expression value limit.
#' 
#' 
#' @param es expression set
#' @param cvlimit coefficient of variance limit
#' @param cv_i coefficient of variance either 'over' or 'under' the index
#' @param exprlimit expression limit numeric value
#' @param exp_i expression index either 'over' or 'under' the index
#' @param pctsample percentage of samples
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #filter <- esFilter(eset, cvlimit = 0.5, cv_i = "over", exprlimit = 2, exp_i = "over", pctsample = 1)
#' #filter
#' 
esFilter <-
function(es, cvlimit = 0.5, cv_i = 'over', exprlimit, exp_i = 'over', pctsample = 1) {
  ############################
  ## Filter function to filter expression set class  
  ## Based on cv of each expression (greater or less than) certain cv limit
  ## and percent of samples with (greater or less than) certain expression value limit
  ###########################
  
  if(exp_i == 'over') { 
    row.selected = apply(exprs(es), 1, function(x) length(c(x > exprlimit)[c(x > exprlimit)== TRUE])/length(x))
    row.selected = row.selected[row.selected>pctsample]
  }
  if(exp_i == 'under') {
    row.selected = apply(exprs(es), 1, function(x) length(c(x < exprlimit)[c(x < exprlimit)== TRUE])/length(x))
    row.selected = row.selected[row.selected>pctsample]
  }
  seles = es[names(row.selected),]  
  cv <- function(x) sd(x)/mean(x)
  
  if(cv_i == 'over') {
    row.cv <- apply(exprs(seles), 1, cv)
    sel.cv = row.cv[row.cv>cvlimit]
  }
  
  if(cv_i == 'under') {
    row.cv <- apply(exprs(seles), 1, cv)
    sel.cv = row.cv[row.cv < cvlimit]
  }
  seles = seles[names(sel.cv),]
  seles
}
