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
