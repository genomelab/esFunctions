esPathwayRun <-
function(es, covar, nsim=10, anovaresults, pathwaytype='c2.cp.kegg') {
  msigdb<-NULL  # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable.  
  # There are 18 pathway choices 
  # eg: 
  # c2.cp.kegg
  # c2.cp.biocarta
  # c3.all
  # c3.mir
    
  # Based on the given pathway type the curatedpathwayfile will be loaded fron the target directory
  curatedpathwayfile <- paste("~//Dropbox//TARGET_PROJECT//data//Pathway_Db//",pathwaytype,".v3.1.symbols.gmt.RData",sep="")
  pathwayinfile <- paste("~//Dropbox//TARGET_PROJECT//data//Pathway_Db//",pathwaytype,".v3.1.symbols.gmt_df.txt",sep="")
  load(curatedpathwayfile)
  
  # Calling esPathwaySig function
  final.results <- esPathwaySig(es=es, covar=covar, nsim=nsim, anovaresults=anovaresults, msigdb)
  
  # Merging pathway results with annova results
  pw_df <- read.table(pathwayinfile,header=T,sep="\t")
  res <- final.results
  data1<-merge(as.data.frame(pw_df), as.data.frame(res), by.x="pathway", by.y="pathway", all.x=TRUE, all.y = TRUE )
  diff_exp_res <- anovaresults 
  colnames(diff_exp_res)[1] = 'genes'
  data<-merge(as.data.frame(data1), as.data.frame(diff_exp_res), by.x="gene_symbol", by.y="genes")
  
  #### Now let's clean up and calculate distances
  groups = colnames(anovaresults)[grep('Chi', colnames(anovaresults))]
  d = data[!is.na(data[groups[1]]),]
  r <- NULL
  x = as.list(unique(d$pathway))
  x = x[!is.na(x)]
  length(x)
  for( i in 1: length(x)) {
    y = d[d$pathway == (x[[i]]),]
    for (selgroup in groups){
      group = sub('Chi_', '', selgroup)
      group1 = strsplit(group,'_')[[1]][1]
      group2 = strsplit(group,'_')[[1]][2]
      mgroup1 = paste0('Mean_', group1)
      mgroup2 = paste0('Mean_', group2)
      sd = paste0('Chi_sd_', group)
      chi = paste0('Chi_', group)
      euc = paste0('ecu_', group)
      score = paste0('pathway_score_', group)
	  pw_chi_sd_euc = paste0('ecu_pw_chi_sd_', group)
      y[[sd]] = sd(y[[chi]])
      y[[euc]] =  sqrt(sum((y[[mgroup1]]-y[[mgroup2]])^2))
      y[[score]] = y[[sd]] * y[[euc]]
	  y[[pw_chi_sd_euc]] =  sqrt(sum((y[[sd]]-y[[euc]])^2))
    }
    r <- rbind(r,y)
  }
  r
}
