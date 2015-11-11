esGetSymbol <-
function(annotatedlist) {
  #########################
  ## Get the symbols from a gene list that has been annotated (i.e. GENE__ProbeId)
  ###########################
  listToCharacterVector(lapply(annotatedlist, function(x) strsplit(x, '__')[[1]][1]))  
}
