#' esGetSymbol
#' 
#' Get the symbols from a gene list that has been annotated (i.e.
#' GENE__ProbeId)
#' 
#' @export
#' @param annotatedlist argument from an annotated list that has gene symbols
#' merged with transcript ids with underscore. For example, some esets will
#' have duplicate genes for various transcript ids, so by calling the function,
#' esAnnotate will output every gene including duplicates. In this case, the
#' row names of the expression set will be as follows "gene_probid".
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #longnmaeset <- esAnnotate(eset, anno)
#' #names_for_cutting <- featureNames(longnmaeset)
#' #head(names_for_cutting) 
#' # OUTPUT:
#' # "gene1_transcriptid1" "gene2_transcriptid2" "gene3_transcriptid3" "gene4_transcriptid4" 
#' # "gene5_transcriptid5" "gene6_transcriptid6"
#' 
#' #genesymbols <-esGetSymbol(names_for_cutting)
#' #head(genesymbols)
#' # OUTPUT:
#' # "gene1" "gene2" "gene3" "gene4" "gene5" "gene6"
#' 
#' 
#' 
#' 
esGetSymbol <-
function(annotatedlist) {
  #########################
  ## Get the symbols from a gene list that has been annotated (i.e. GENE__ProbeId)
  ###########################
  listToCharacterVector(lapply(annotatedlist, function(x) strsplit(x, '__')[[1]][1]))  
}
