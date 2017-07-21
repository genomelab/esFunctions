#' esCreate
#' 
#' Create an ExpressionSet Class from gene expression matrix and covariate
#' file. \cr Column names of gene expression matrix must match the row names of
#' covariate file.
#' 
#' 
#' @param aptfile Gene expression matrix as a dataframe
#' @param covfile Covariate file describes the sample information
#' @param match Match is set to false and check if the column names of gene
#' expression matrix match row names of the covariate file. If match is TRUE,
#' it will not check.
#' @param match_covcol By default, the function looks for the matches of the
#' column assay to the rows of the covariate dataframe. But match covariate
#' column, specify whether you want to match the column of the expression
#' matrix to a column of the covariate dataframe (instead of matching a row in
#' a covariate dataframe).
#' @param annot Annot is a annotation dataframe that has transcript_ids,
#' probeset_ids, reference gene symbols, etc...
#' @param annotate This argument is like a flag that if it is specified, it
#' will call esAnnotate function with the annot as the argument
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #neweset<-esCreate(assaydata, covariateframe, match = FALSE)
#' #View(neweset)
#' 
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
