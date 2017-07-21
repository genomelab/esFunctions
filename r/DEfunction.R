#' DEfunction
#' 
#' Differential expression comparing each level to the others
#' 
#' 
#' @param y is a vector of data
#' @param var1 covariate
#' @param contrastmodel %% ~~Describe \code{contrastmodel} here~~
#' @author Shahab Asgharzadeh
DEfunction <-
function(y, var1, contrastmodel = NULL) {
  ############################################################
  ### 
  ###  esDE  (differential expression comparing each level to the others)
  ###  y is a vector of data
  ###  var1 is the covariate 
  ###  This function is called from de_analysis
  ###########Not Done
  
  ###################################
  ### Using var1 as the covariate and find all pairwise comparisions
  ###################################
  
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
  
  PVALUE <- data.frame(rbind(PVALUE1,PVALUE2))	
  colnames(PVALUE) = c("Chi2","p_value","fold_changee","score","abs_score")
  
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
  
  result<-c(x2,pvalue,replace1,replace2)
  
  rn <- list()
  for( i in 1: n){
    n1 <- paste("Mean_",levels(var1)[i], sep="")
    rn <- rbind(rn, n1)
  }
  for(i in 1: n){
    n1 <- paste("SE_",levels(var1)[i], sep="")
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
  
  names(result) = c("x2", "pvalue", rn, pn )
  
  
  ###################################
  ### Using var2 as the covariate and find all combined comparisions
  ################################### 
  
  
  for ( j in 1: length(levels(var1))){
    
    R <- levels(var1)[j]
    
    L <- levels(var1)[levels(var1)!=R]
    
    var2 <- as.character(var1)
    for ( i in 1:length(var1)){
      if (var1[i] %in% L)  {(var2[i] = "Combine") }
      else {(var2[i] = R)}
    }
    var2 <- factor(var2)
    eset <- esAddPheno(eset,var2)
    
    
    # Fit H1 and H0 models
    if (length(contrastmodel) == 0){
      contrast = 0
    }
    
    g1<-glm(y~eset$var2)
    
    # Compute x2 and p-value
    x2<-g1$"null.deviance"-g1$deviance
    pvalue<-1-pchisq(x2,df=2)
    
    # Extract means and SEs
    coeff<-summary(g1)$coefficient[,1]
    vc<-summary(g1)$cov.scaled
    Mean_1 <-coeff[1]
    Mean_2 <-coeff[1]+coeff[2]
    
    SE_1<-sqrt(vc[1,1])
    SE_2 <-sqrt(vc[1,1]+vc[2,2]+2*vc[1,2])
    
    # Compute p-values for HR:A vs N and N: 1 vs HR
    
    x2_2_1 <-coeff[2]^2/vc[2,2]
    p_2_1 <-1-pchisq(x2_2_1 ,df=1)
    fc_2_1 = Mean_2 - Mean_1
    score_2_1 = fc_2_1 * x2_2_1
    abs_score_2_1 = abs(score_2_1)
    
    # Store and return
    
    
    x<-c(x2,pvalue,x2_2_1,p_2_1,fc_2_1, score_2_1, abs_score_2_1)
    names = c("Chi_2_1","P_2_1", "fc_2_1", "score_2_1", "abs_score_2_1")
    names = sub("1",levels(var2)[1],names)
    names = sub("2", levels(var2)[2], names)
    names(x) = c(paste0("x2_Combine_",R), paste0("pvalue_Combine_",R), names)
    
    result <- c(result,x)
    
  }
  result
}
