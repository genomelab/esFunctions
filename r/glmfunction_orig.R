#' General Linear Model
#' 
#' Glmfunction calls the in-house "glm" function to include p-value,
#' chi-squared, means, but does not compare between groups. The calculated
#' values are only done for a single vector of data.
#' 
#' 
#' @param y the vector of data
#' @param var1 the covariate (n levels) for ANOVA analyses
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #x <- c(2,3,4,7,9,4,10,6,4,2)
#' #y <- as.factor(c("a","b","c","a","b","c","b","a","c","c"))
#' #results <- glmfunction_orig(x,y)
#' #results 
#'  
#' 
#' 
glmfunction_orig <-
function(y, var1) {
  ############################################################
  ###  glmfunction
  ###  y is a vector of data
  ###  var1 is the covariate (n levels) for ANOVA analyses
  ###  base is the value within var1 to use as the base level
  ###  This function is called from anova_analysis
  ############################################################  
  n <- length(levels(var1))
  # Fit H1 and H0 models
  g1<-glm(y~var1)
  
  # Compute x2 and p-value
  x2<-g1$"null.deviance"-g1$deviance
  pvalue<-1-pchisq(x2,df=n-1)
  
  # calculate the number of possible contrasts 
  con <- factorial(n)/(factorial(n-2)*2)
  
  # Extract means and SEs
  coeff<-summary(g1)$coefficient[,1]
  vc<-summary(g1)$cov.scaled
  
  result <- matrix(ncol=2, nrow=n)
  result[1,1] <-coeff[1]
  result[1,2]<-sqrt(vc[1,1])
  for( i in 2:n){
    result[i,1] <-coeff[1]+coeff[i]
    result[i,2] <-sqrt(vc[1,1]+vc[i,i]+2*vc[1,i])
  }
  colnames(result) <- c("mean","se")
  rownames(result) <- levels(var1)
  
  # Compute p-values 
  
  # just for 1 vs. i
  PVALUE1 <- matrix(ncol=5, nrow=n-1)
  for( i in 2:n){
    xl <-coeff[i]^2/vc[i,i]
    PVALUE1[i-1,1] <- xl  
    PVALUE1[i-1,2] <-1-pchisq(xl ,df=1)
    fc <- result[i,1] - result[1,1]
    PVALUE1[i-1,3] <- fc
    score <- fc * xl
    PVALUE1[i-1,4] <- score
    PVALUE1[i-1,5] <- abs(score)
  }
  
  # for i vs. j  
  m = 0
  PVALUE2 <- matrix(ncol=5, nrow= con-(n-1)) 
  if(n>2) {
    for( i in 2:(n-1)){
      for(j in (i+1):n){
        m= m+1
        xl <-(coeff[i]-coeff[j])^2/(vc[i,i]+vc[j,j]-2*vc[i,j])
        PVALUE2[m,1] <- xl  	 
        PVALUE2[m,2] <-1-pchisq(xl ,df=1)
        fc <- result[j,1] - result[i,1]
        PVALUE2[m,3] <- fc
        score <- fc * xl
        PVALUE2[m,4] <- score
        PVALUE2[m,5] <- abs(score)
      }
    }	
  }  
  
  
  PVALUE <- data.frame(rbind(PVALUE1,PVALUE2))	
  colnames(PVALUE) = c("chi2","p_value","fold_changee","score","abs_score")
  
  matname <- data.frame(levels(var1))
  row_names <- list()
  for( i in 1:(n-1)){
    for(j in (i+1):n){
      pp <- paste(matname[i,1],"_", matname[j,1], sep="")
      row_names <- rbind(row_names, pp)
    }
  }
  rownames(PVALUE) <- row_names	
  
  # Store and return
  
  # final_result <- list(table1=result, table2=PVALUE)
  # final_result 
  
  replace1 <- result
  replace2 <- as.matrix(PVALUE)
  dim(replace1) = NULL
  dim(replace2) = NULL
  
  x<-c(x2,pvalue,replace1,replace2)
  
  rn <- list()
  for( i in 1: n){
    n1 <- paste("Mean_",rownames(result)[i], sep="")
    rn <- rbind(rn, n1)
  }
  for(i in 1: n){
    n1 <- paste("SE_",rownames(result)[i], sep="")
    rn <- rbind(rn, n1)
  }	
  
  pn  <- list()
  for( i in 1: con){
    n1 <- paste("Chi_",rownames(PVALUE)[i], sep="")
    pn <- rbind(pn, n1)
  } 
  for( i in 1: con){
    n1 <- paste("P_",rownames(PVALUE)[i], sep="")
    pn <- rbind(pn, n1)
  }
  for( i in 1: con){
    n1 <- paste("fc_",rownames(PVALUE)[i], sep="")
    pn <- rbind(pn, n1)
  }
  for( i in 1: con){
    n1 <- paste("score_",rownames(PVALUE)[i], sep="")
    pn <- rbind(pn, n1)
  }
  for( i in 1: con){
    n1 <- paste("abs_score_",rownames(PVALUE)[i], sep="")
    pn <- rbind(pn, n1)
  }	
  
  names(x) = c("x2", "pvalue", rn, pn )
  x
}
