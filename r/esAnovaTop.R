#' esAnovaTop
#' 
#' An arrange of inputs. It will take the best results from the argument that
#' is given through sortby.
#' 
#' @export
#' @param es expression set
#' @param anovaresults dataframe from esanova
#' @param range Amount of the top results you want
#' @param sortby column name you want from esanova
#' @param equal Include values that are equal
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #results <- esAnovaTop(eset, anova_table, range = c(1:some_number), #sortby = "anova_column_interest", equal = FALSE)
#' 
esAnovaTop <-
function(es, anovaresults, range, sortby='abs_score', equal=FALSE) { 
  if(!equal) {
    sortinglist = names(anovaresults)[grep(sortby, names(anovaresults))]
    findtoptranscripts = function(slist, range) {
      slist = anovaresults[[slist]]
      sorted = anovaresults[order(-slist),]$id
      features = unique (as.character(sorted))[range]
      return(features)
    }
    features = unique(unlist(lapply(sortinglist, function(x) findtoptranscripts(x, range))))
  }
  if(equal){
    sortinglist = names(anovaresults)[grep(sortby, names(anovaresults))]
    findtoptranscripts = function(slist, range) {
      if(strsplit(slist,split='_')[[1]][1] == 'abs') 
        groups = paste0('score_', strsplit(slist,split='_')[[1]][3], '_',strsplit(slist,split='_')[[1]][4])
      else
        groups = paste0('score_', strsplit(slist,split='_')[[1]][2], '_',strsplit(slist,split='_')[[1]][3])
      #return(sprintf(groups))
      slist1 = anovaresults[anovaresults[[groups]]<0,]
      slist2 = anovaresults[anovaresults[[groups]]>0,]
      sorted1 = slist1[order(-slist1[[slist]]),]$id
      sorted2 = slist2[order(-slist2[[slist]]) ,]$id
      features = unique(c(as.character(sorted1[range]), as.character(sorted2[range])))
      return(features)
    }
    features = unique(unlist(lapply(sortinglist, function(x) findtoptranscripts(x, range))))
  }
  features
}
