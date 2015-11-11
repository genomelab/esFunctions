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
