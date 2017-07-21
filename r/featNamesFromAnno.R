#' featNamesFromAnno
#' 
#' Feature names of an expression set from annotation
#' 
#' 
#' @param annovec annotation vector
#' @param ann2featMap annotation2map
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
featNamesFromAnno <-
function( annovec, ann2featMap ) {
  if (is(ann2featMap, "AnnDbBimap")) 
    anslist =  AnnotationDbi::mget( annovec, ann2featMap, ifnotfound=NA ) 
  else if (is(ann2featMap, "list")) 
    #anslist = ann2featMap[ match(annovec, names(ann2featMap)) ]
    anslist = ann2featMap[ names(ann2featMap) %in% annovec ]
  lens = sapply(anslist,length)
  nn = rep(names(anslist), lens)
  ans = unlist(anslist)
  names(ans) = nn
  ans
}
