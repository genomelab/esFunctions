#' jetColors
#' 
#' This relates to the color settings
#' 
#' 
#' @param N %% ~~Describe \code{N} here~~
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
jetColors <-
function(N){
  k <- ceiling(N/4)
  temp.red <- c(rep(0,2*k), 1:k, rep(k,k-1), k:1)
  temp.green <- c(rep(0,k), 1:k, rep(k,k-1), k:1, rep(0,k))
  temp.blue <- c(1:k, rep(k,k-1), k:1, rep(0,2*k))
  temp.rgb <- cbind(temp.red, temp.green, temp.blue)
  delta <- 5*k-1 - N
  delta <- ceiling(delta/2)
  temp.rgb <- temp.rgb[delta:(delta+N-1),]/k
  
  ## assemble everything last value is returned
  rgb(temp.rgb[,1], temp.rgb[,2], temp.rgb[,3])
}
