#' df_factor
#' 
#' Takes a dataframe and makes each column a factor, returns a dataframe
#' 
#' 
#' @param df data frame
#' @author Shahab Asgharzadeh
df_factor <-
function(df) {
  ## Takes a dataframe and makes each column a factor, returns a dataframe
  
  df2 = as.data.frame(sapply(df, function(x) x = droplevels(as.factor(x))))
  df2
}
