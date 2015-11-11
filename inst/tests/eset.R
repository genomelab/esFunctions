library("Biobase") #loads the library biobase
library("reshape")
library("ggplot2")
source("http://bioconductor.org//biocLite.R")
biocLite("Hmisc")

#Reading in the files -------
#sourcing the data files means that we do not need to reattach all the codes and can just use them.

source("/Users/anthonycolombo60/Documents/Programming Languages/R Language/Expression Sets/esfunctions_ver413.r")


exprsnfile<- "/Users/anthonycolombo60/Documents/Programming Languages/R Language/Expression Sets/Expression Files/core_transcripts_filtered_expression.txt"

assay<-as.matrix(read.table(exprsnfile, header = TRUE, sep="\t", row.names=1,as.is=TRUE))
#-------------End of reading in the assay data--------------------


#NOW READING IN THE COVARIATE MATRIX-----------


pdatafile<- "/Users/anthonycolombo60/Documents/Programming Languages/R Language/Expression Sets/Expression Files/Master Covariates_05_23_13.txt"

#ROW.NAMES NOTES
#row.names =1 means that the row names are named after the first row entry, if there any duplicates it will error out. and cause issues
#remove all duplicates.

pData<-read.table(pdatafile,row.names=1, header=TRUE, sep="\t")
#----------------End of reading in the cov data-------------------------






####MAIN SCRIPT - note the functions can actually now be cut out and I can just source the code

#####beginning with the main script



#checking all the dimensions and general

class(assay)

dim(assay)

head(assay[, 1:5])
rownames(assay)


class(pData)
dim(pData)
head(pData[,1:5])

rownames(pData)
colnames(assay)


assaycolnames <- colnames(assay)


######---- finished checking the general information




#new storing data frame
new_pData <- NULL

#need to find the rownames(pData) ==colnames(assay)  preparation for creating the eset because
# we need to align the columns of the assay to the rows of the cov matrix

for (i in 1:length(assaycolnames)){
  index<- which( assaycolnames[i]==rownames(pData))
  x<-pData[index,]
  new_pData <- rbind(new_pData, x)
}
all(rownames(new_pData)==colnames(assay))   #this yields true, so the we can create the exprs matrx

#creating an annotated covariate matrix

phenoData<- new("AnnotatedDataFrame", data=new_pData)

#assembling the expression set

eset<- ExpressionSet(assayData=assay, phenoData=phenoData, dimLabels = c("rowNames", "columnNames"))


##ANNOTATING A MATRIX FROM SCRATCH


#ANNOTATING THIS EXPRESSION SET
#notes --- annotating the expression set means that we take the rows of the assay data and combine them with the rows of the annotatin matrix
#rownames(eset) will output many numbered entries, we want to have the output be linked with gene names.
#we need to use the merge function with another data set
# to load the annotation matrix, these genetic information has already been created
#load("/Users/anthonycolombo60/Documents/R Language/Expression Sets/Expression Files/huex_anno_tsr.Rdata")


#note WE ARE NOT LOADING A DATA FILE, BUT AN R OBJECT.  THAT IS WHAT .RDATA IS , IT IS AN OBJECT
#an .txt file is an information file.  
infilepath<- "/Users/anthonycolombo60/Documents/Programming Languages/R Language/Expression Sets/Expression Files/huex_anno_tsr.Rdata"
outfilepath<-"/Users/anthonycolombo60/Documents/Programming Languages/R Language/Expression Sets/Expression Files/huex_anno_tsr.txt"
load(infilepath) #this creates an object

#note there are two types of files, objects and .txt that we load



#the annotation matrix has two columns, it has a probe_set_ID and a transcript_ID.  The array is linked to the 
#transcript IDs and not the probesetIDs. the probesetIDs are genereated first, and then the data
#is transferred over to the transcriptIDs which reduces the data from millions of data points to thousands
#this cleans up the data and makes it more easy to work from.  then from the transcript IDs we create the gene expression labels

#the process to annotate a matrix, The annotated matrix is made from Expression1.R file the featureNames(eset) will be the gene names
#in order to do this create a new data.frame by eliminating the postscriptiDs.  Then we merge the new dataframe to the expression set, linking together all the transcriptIDs
#after linking together the transcriptsIDS we delete this row (should be the first row), and then the new featureNames should be the gene names.
#after linking together the gene names, we search for duplicates, and if there are duplicates, we average the assay data of both of them, and then delete the dupliates

#removing the postscriptIDs and all of the other data
annotrans<-annotation_HuEx_tsr$transcript_cluster_id
anno<-annotation_HuEx_tsr[,c(1,20)]
class(anno)
View(anno) #see that the all the data is removed except for the transcriptID's and the ref_gene_symbol
#now the annotated data frame is created, and need to attach this to matching IDs in the eset

#checking the features of eset
featureNames(eset)

#now we need to match the transcript IDs. the problem is that the matrices are of different sizes.  need to check ith at a time
length(rownames(anno)) #337842 total entries
length(rownames(eset)) #17560   total entries

#this loads all the object ready to be annotated by esAnnotate(es,anno) which then maps transcript IDs to GeneIDs.
