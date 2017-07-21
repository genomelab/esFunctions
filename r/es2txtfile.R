#' es2txtfile
#' 
#' Converts Expression set data to .txt format that can be used to upload to
#' MySQL. Creates the Class label file (.cls) using the covar variable
#' provided. The covar label must be in the phenotype data of es.
#' 
#' @export
#' @param es expression set
#' @param exprfilename expression set file name that you will write to a table,
#' but only the expression assay data. Requires (\code{.txt})
#' @param covarfilename covariate file name that requires (\code{.cls})
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #text_file <- es2txtfile(eset, "smallassay.txt", "smallpheno.cls")
#' #text_file
#' # Writes to current working directory 
#' #getwd()
#' # Files will be on that location 
#' 
es2txtfile <-
function(es, exprfilename, covarfilename) {
  ###  Converts Expression set data to .txt format that can be used to upload to MySQL
  ##   Creates the Class label file (.cls) using the covar variable provided 
  ##   The covar label must be in the phenotype data of es.
  
  ## Create Expresison Table File
  output = as.data.frame(exprs(es))
  
  write.table(output,file=exprfilename, quote=FALSE, sep='\t', row.names=TRUE, col.names=NA)
  
  ## Create covariate file
  pheno = pData(es)
  pheno$Row.names = rownames(pheno)
  colnames(pheno)[1] = 'samplename'
  write.table(pheno,file=covarfilename, quote=FALSE, sep='\t', row.names=TRUE, col.names=NA)
}
