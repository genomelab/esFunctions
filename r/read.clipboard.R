#' read.clipboard
#' 
#' this is for reading and copying inputs
#' 
#' 
#' @param header %% ~~Describe \code{header} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
read.clipboard <-
function(header=T, ...) {
  
  ###########################
  ## read from clipboard (useful for macs only?)
  ###########################
  read.delim(pipe('pbpaste'), header=header, ...)
}
