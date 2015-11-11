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
