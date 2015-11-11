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
