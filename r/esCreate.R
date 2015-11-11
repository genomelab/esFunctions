esCreate <-
function(aptfile, covfile, match = FALSE, match_covcol = 'rownames', annot = "optional", annotate = FALSE) {
  ##################################################
  ## Function 1
  ## Create ExpressionSet Class from 
  ## APT output file and covariate file
  ## Assumption: column names of expression set are
  ##             the same (or subset) of the column
  ##             names of covariate file
  #################################################
  if(!is.data.frame(covfile)) {
  data <- as.matrix(read.table(aptfile,sep="\t",header=T,row.names=1))
  cov <- data.frame(read.table(covfile,sep="\t",header=T))}
  else
  {
  data = aptfile
  cov = covfile
  }
  if (!match) {  ## Assuming the expresion and covariates have correct samples and order
    phenoData <- new("AnnotatedDataFrame", data=cov)
    eset<- new("ExpressionSet", exprs=data, phenoData=phenoData)
    return(eset)
  }
  else
  {
  #colnames(data) 
  #rownames(cov)
  if (match_covcol == '') {match_covcol = colnames(cov[1])  
  cov = cov[!duplicated(cov[match_covcol]),]
  rownames(cov) = unlist(cov[match_covcol])
  }
  
  grepme = function(x) grep(x, rownames(cov))
  order = unlist(sapply(colnames(data), grepme))
  match = rownames(cov[order,])
  
  grepnomatch = function(x) grep(x, rownames(cov), invert = TRUE)
  lastorder = sapply(colnames(data), grepnomatch)
  nomatch = colnames(data)[sapply(lastorder, function(x) length(x)!=length(colnames(data)))]
  nomatch_location = unlist(sapply(nomatch, function(x) grep(x, colnames(data))))
  
  cov = cov[order,]
  
  for(i in 1:(length(nomatch))) 
    match <- append(match, nomatch[i], after=(nomatch_location[i])-1)
  
  ### Create extra rows based on the unmatches of column data to the covariate info
  cov_nomatch = cov[1:length(nomatch),]
  rownames(cov_nomatch) = nomatch
  cov_nomatch[] = "NULL"
  cov = rbind(cov, cov_nomatch)
  
  #### Order based on the columns of the data
  cov = cov[match(match, rownames(cov)),]
  colnames(data) = match
  
  phenoData <- new("AnnotatedDataFrame", data=cov)
  eset<- new("ExpressionSet", exprs=data, phenoData=phenoData)
  if(annotate) {
    esAnnotate(eset, annot)
  }
  sprintf(paste("No covariate matches for data column: ", nomatch))
  return(eset)
  }
}
