#' revAnnoEnv
#' 
#' this gives a revises the input.  It takes an input, creates a list of
#' objects, prints the names of the input,
#' 
#' 
#' @param env envelope
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
revAnnoEnv <-
function(env) {
  li = as.list(env)
  na = names(li)
  lens = sapply(li,length)
  rna = rep(na,lens)
  newbas = as.character(unlist(li))
  split(rna, newbas)
}
