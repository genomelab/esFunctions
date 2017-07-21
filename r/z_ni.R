#' ni
#' 
#' Setting color pallette for ggplot and to setup colors for heatmap
#' 
#' 
#' @param x this is an input, where the usage takes whatever is not in x and is
#' in y
#' @param y the second compared input.
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
`ni` <-
function(x, y) x[!x %in% y]
