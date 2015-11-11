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
