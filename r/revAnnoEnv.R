revAnnoEnv <-
function(env) {
  li = as.list(env)
  na = names(li)
  lens = sapply(li,length)
  rna = rep(na,lens)
  newbas = as.character(unlist(li))
  split(rna, newbas)
}
