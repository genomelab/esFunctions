#' fixpath
#' 
#' this creates a path to the data
#' 
#' 
#' @param x this is the input
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
fixpath <-
function(x) {
  set_tmp_path_data() 
  #### Adds the setwd to the filename  (some issues with relative directory in some systems)
  paste0(gsub('/','//',getwd()),substring(x,first=2,last=1000000L))
}
