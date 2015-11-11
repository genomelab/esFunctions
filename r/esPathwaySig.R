esPathwaySig <-
function(es, covar, nsim=10, anovaresults, msigdb) {
  
  gene.id<-NULL # # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable. 
  pathway<-NULL # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable.  
  
  
  ###### Pairwise comparison of groups (identified in covar) using es 
  ### nsim<-10000   - number of simulations to pickup FDR for the pathways
  ### pathwaytype:  'cp' canonical, 'pos' genomic positions
  ### anovaresults from the es given, rownames must be gene names
  ### Example of use:
  ###    myanova = esAnova(es=selexprset, covar=selexprset$NMF3_cluster)
  ###    geresults = esPathwaySig(es=selexprset, covar='NMF3_cluster',nsim=10,anovaresults=myanova,pathwaytype='cp')
  ###    esPathwayplot(geresults, c("1", "2"), c(1:2))   ## Plots the difference between the groups for the pathway (euclidean distance) vs. SD of significance for the genes in the pathway
  
  # Make normalized dataframe out of list
  
  # Total number of elements
  m<-length(unlist(msigdb))
  
  # Number of  pathways
  n<-length(names(msigdb))
  
  xpp<-matrix("",ncol=2,nrow=2*m)
  icount<-0
  for (i in (1:n) ) {
    for (j in (1:length(msigdb[[i]]))) {
      icount<-icount+1
      xpp[icount,]<-c(names(msigdb)[i],msigdb[[i]][j])
    }
  }
  
  # Convert to dataframe
  xpp<-as.data.frame(xpp,stringsAsFactors=FALSE)
  names(xpp)<-c("pathway","gene.id")
  xpp<-subset(xpp, gene.id!="") #the matrix was initialized too large. Get rid of the empty rows.
  
  # get rid of repeated genes in a pathway
  pathway.unique<-matrix("",ncol=2,nrow=m)
  pathways<-unique(xpp$pathway)
  jcount<-0
  for (i in 1:length(pathways)) {
    geneset<-subset(xpp, pathway==pathways[i])$gene.id
    genes<-(unique(geneset))                    #unique gene names in the pathway
    for (j in 1:length(genes)) {
      jcount=jcount+1
      pathway.unique[jcount,]<-c(pathways[i],genes[j])
    }
  }
  
  colnames(pathway.unique)<-c("pathway","gene.id")
  pathway.unique<-as.data.frame(pathway.unique)
  pathway.unique<-subset(pathway.unique, gene.id!="") #the matrix was initialized too large. Get rid of the empty rows.
  
  # Clean up trash
  rm(list=c("i","j","n","m","icount","xpp","msigdb", "genes", "geneset", "jcount", "pathways"))
  
  ##############################################################################################
  # Get the two groups of Expression data for GSEA analysis
  ##############################################################################################
  exprs<-anovaresults
  
  
  ##############################################################################################
  # label whether a gene is on the chip or not, get rid of empty pathways  #####################
  ##############################################################################################
  onChipOrNot<-pathway.unique$gene.id%in%rownames(exprs)
  names(onChipOrNot)<-c("onChipOrNot")
  pathway.unique<-cbind(pathway.unique,onChipOrNot)
  
  # get rid of pathways that do not have any genes in the expreset
  pathway.unique.2<-matrix("",ncol=3,nrow=dim(pathway.unique)[1])
  pathways<-as.vector(unique(pathway.unique$pathway))
  jcount<-0
  for (i in 1:length(pathways)) {
    genes<-as.vector(subset(pathway.unique, pathway==pathways[i])$gene.id)
    onChip<-subset(pathway.unique, pathway==pathways[i])$onChipOrNot
    if (sum(onChip)>0) {
      for (j in 1:length(genes)) {
        jcount=jcount+1
        pathway.unique.2[jcount,]<-c(pathways[i],genes[j],onChip[j])
      }
    }
  }
  
  colnames(pathway.unique.2)<-c("pathway","gene.id", "onChipOrNot")
  pathway.unique.2<-as.data.frame(pathway.unique.2)
  
  pathway.unique<-subset(pathway.unique.2, gene.id!="") #the matrix was initialized too large. Get rid of the empty rows.
  
  
  ############### GSEA #######################################################################
  
  all.gsea.results = list()
  ###### number of unique pathways ######
  pathways<-as.vector(unique(pathway.unique$pathway))
  groups = colnames(anovaresults)[grep('Chi', colnames(anovaresults))]
  
  for(selgroup in groups) {
    
    print(selgroup)
    ###### the original observed T statistic ######
    obs.all <- sqrt(exprs[[selgroup]])
    names(obs.all)<-rownames(exprs)
    
    ###### initialize the matrix ######
    sample.tt <- matrix(nrow=length(pathways),ncol=nsim+1+2)
    colnames(sample.tt)<-c("AllGeneNum","CoreGeneNum","ObsMeanT",seq(1,nsim))
    rownames(sample.tt)<-pathways
    
    sample.tt.2sided <- matrix(nrow=length(pathways),ncol=nsim+1+2)
    colnames(sample.tt.2sided)<-c("AllGeneNum","CoreGeneNum","ObsMeanT",seq(1,nsim))
    rownames(sample.tt.2sided)<-pathways
    
    ###### do random sampling ######
    title<-c("Starting analysis using random sampling: ")
    print(title)
    
    for (i in 1:length(pathways)) {
      genes<-subset(pathway.unique, pathway==pathways[i])$gene.id  #all genes in a pathway
      iselect<-names(obs.all)%in%genes        		   #a logical vector specifying whether a gene is in the pathway
      
      all_tscore<-obs.all 		                                 #all the observed T statistics
      onChip<-all_tscore[iselect] 	           				   #T statistics for genes in a pathway
      
      sample.tt[i,1]<-length(genes)                   		   #the number of all genes in the pathway
      sample.tt[i,2]<-length(onChip)	           			   #the number of core genes in the pathway
      sample.tt.2sided[i,1]<-length(genes)          	 		   #the number of all genes in the pathway
      sample.tt.2sided[i,2]<-length(onChip)	   		   	   #the number of core genes in the pathway
      
      ###### The T score for the original samples ######
      sample.tt[i,3]<-mean(onChip)
      sample.tt.2sided[i,3]<-mean(abs(onChip))
      
      ###### randomly sample nsim times ######
      for (k in 1:nsim) {
        sample.random<-sample(all_tscore,length(onChip))
        sample.tt[i,(k+3)]<-mean(sample.random)
        sample.tt.2sided[i,(k+3)]<-mean(abs(sample.random))
        
      }
    }
    print(paste0("Pathways Analyzed = ",i))
    
    ###### Calculate the number of statistics that are smaller//bigger than observed ######
    print(selgroup)
    title<-c("Get p values")
    print(title)
    sample.tt.s <- matrix(0,nrow=length(pathways),ncol=3)
    sample.wil.s <- matrix(0,nrow=length(pathways),ncol=3)
    rownames(sample.tt.s)<-pathways
    colnames(sample.tt.s)<-c("Upregulated","Downregulated", "Twosided")
    rownames(sample.wil.s)<-pathways
    colnames(sample.wil.s)<-c("Upregulated","Downregulated", "Twosided")
    
    for (k in 1:length(pathways)) {
      for (l in 1:nsim) {
        ncol<-(l+3)
        if (sample.tt[k,3]>=sample.tt[k,ncol]) {
          sample.tt.s[k,1]<-sample.tt.s[k,1]+1
        }	
        if (sample.tt[k,3]<=sample.tt[k,ncol]) {
          sample.tt.s[k,2]=sample.tt.s[k,2]+1
        }
        if (sample.tt.2sided[k,3]>=sample.tt.2sided[k,ncol]) {
          sample.tt.s[k,3]=sample.tt.s[k,3]+1
        }
      }
    }
    
    sample.tt.s2<-cbind(sample.tt[,1:3],sample.tt.s)
    sample.tt.s2[,4:6]<-1-(sample.tt.s2[,4:6]/nsim)
    
    ########### compute FDR adjusted p value #############
    fdr.f<-function(x,y) {
      obs.p<-as.matrix(x)
      rownames(obs.p)<-rownames(y)
      p.adjusted<-matrix(0,nrow=length(obs.p[,1]),ncol=1)
      obs.p.sort<-sort(obs.p[,1],index.return=TRUE)
      for (i in length(obs.p.sort$x):1) {
        p.adjusted[i,1]=obs.p.sort$x[i]*length(obs.p.sort$x)/i	
        if (i!=length(obs.p.sort$x)) {
          if (p.adjusted[i,1]>=p.adjusted[(i+1),1]) {
            p.adjusted[i,1]=p.adjusted[(i+1),1]
          }	
        }
      }
      rownames(p.adjusted)<-names(obs.p.sort$x)
      temp<-merge(as.data.frame(y), as.data.frame(p.adjusted), by.x=0, by.y=0)
      rownames(temp)<-temp[,1]
      temp[,2:length(temp[1,])]
    }
    sample.tt.s3<-fdr.f(sample.tt.s2[,4], sample.tt.s2)
    sample.tt.s3<-fdr.f(sample.tt.s3[,5], sample.tt.s3)
    sample.tt.s3<-fdr.f(sample.tt.s3[,6], sample.tt.s3)
    colnames(sample.tt.s3)<-c("AllGeneNum","CoreGeneNum","ObsMeanT","UpRegulated.p","DownRegulated.p","DeRegulated.p","UpRegulated.FDR","DownRegulated.FDR","DeRegulated.FDR")
    sample.tt.s3<-sample.tt.s3[,c("AllGeneNum","CoreGeneNum","UpRegulated.p","DownRegulated.p","DeRegulated.p","UpRegulated.FDR","DownRegulated.FDR","DeRegulated.FDR")]
    gsea.results<-sample.tt.s3
    print(selgroup)
    all.gsea.results[[which(groups==selgroup)]] = gsea.results
    
  }
  names(all.gsea.results) = groups
  print("All done ...")
  
  ##################################################################################################################
  ######## Organize results ########################################################################################
  ##################################################################################################################
  
  counter = 1
  for(selgroup in groups) {  
    results = all.gsea.results[[selgroup]]
    colnames(results)<-c("AllGeneNum", "CoreGeneNum", "UpRegulated.p", "DownRegulated.p", "DeRegulated.p", "UpRegulated.FDR", "DownRegulated.FDR", "DeRegulated.FDR")
    results<-results[,c("AllGeneNum", "CoreGeneNum", "DeRegulated.p", "DeRegulated.FDR")]
    colnames(results)<-c("AllGeneNum", "CoreGeneNum", paste0(sub('Chi_','',selgroup),".p"), paste0(sub('Chi_','',selgroup),".FDR"))
    if (counter==1) final.results = results else final.results = cbind(final.results, results)
    counter = counter+1
  }
  final.results$pathway = row.names(final.results)
  final.results
}
