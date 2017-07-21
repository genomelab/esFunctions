#' es2GSEAformat
#' 
#' Converts Expression set data to .txt format that can be used in Gene Set
#' Enrichment Analysis (GSEA), a format that is used for differential
#' expression analysis, from BROAD Institute. Creates the Class label file
#' (.cls) using the covar variable provided .
#' 
#' 
#' @param es expression set
#' @param covar The covar label must be in the phenotype data of es.
#' @param exprfilename expression set file name that requires \code{.txt}
#' @param classfilename class label file name that requires \code{.cls}
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' # es2GSEAformat(eset, covar="features_of_interest", "filename.txt", "filename.cls")
#' # Writes to current working directory 
#' # getwd()
#' # Files will be on that location 
#' 
#' 
es2GSEAformat <-
function(es, covar, exprfilename, classfilename) {
  ###  Converts Expression set data to .txt format that can be used in GSEA from BROAD
  ##   Creates the Class label file (.cls) using the covar variable provided 
  ##   The covar label must be in the phenotype data of es.
  output = as.data.frame(exprs(es))
  output$NAME = row.names(output)
  output$DESCRIPTION = row.names(output)
  l = length(names(output))
  output = output[,c(l-1, l, 1:(l-2))]
  write.table(output,file=exprfilename, quote=FALSE, sep='\t', row.names=FALSE)
  
  ## Create .cls file
  output = es[[covar]]
  line1 = paste(length(output), length(levels(droplevels(factor(output)))), '1', sep=' ')
  line2 = paste("#", paste(levels(droplevels(factor(output))), collapse=" "))
  cat(paste(line1, line2, paste(output, collapse=" "), sep='\n'), 
      file=classfilename, sep="")
}
