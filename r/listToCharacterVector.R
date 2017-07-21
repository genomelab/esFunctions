#' listToCharacterVector
#' 
#' this will change the list to a character vector
#' 
#' 
#' @param lst %% ~~Describe \code{lst} here~~
#' @param sep separator link
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
listToCharacterVector <-
function (lst, sep = " //// ") {
  isna <- is.na(lst)
  v <- rep(NA, length(lst))
  if (sum(isna) != length(isna)) 
    v[!isna] <- sapply(lst[!isna], function(x) {
      paste(x, collapse = sep)
    })
  v
}
