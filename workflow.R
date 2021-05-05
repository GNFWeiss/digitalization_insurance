source("auxiliaryFunctions.R")

### Importing and preprocessing of the textual documents
files <- list.files(<directoryPathAnnualReports>) # Documents have to be provided in pdf format.
read<-readPDF(engine="xpdf") # Plain text is retrieved based on the Xpdf extraction engine.
annualReports<- Corpus(URISource(paste0(<directoryPathAnnualReports>,files[])), readerControl = list(reader = read))
refDoc<-Corpus(URISource(<filePathReferenceDocument>), readerControl = list(reader = read)) # Import the reference document on digitalization

# Preprocessing of the textual data
corpCleaned<-cleanCorpus(annualReports)

# Derive a document term matrix
dtm<-DocumentTermMatrix(corpCleaned)

# remove words that appear less than 25 times in total over all reports
dtm<-dtm[,-which(colSums(as.matrix(dtm))<25)]



### Calculate the Latent Dirichlet Allocation (LDA) for various numbers of topics based on the topicmodels package
library(topicmodels)
library(parallel)

numberOfTopics<-seq(15,60,by=15)  # specify number of topics to perform the lda for

# Use parallel functionality to speed up calculations
ldaList<-mclapply(as.list(numberOfTopics),function(k){LDA(x=dtm,k=k, control=list(seed=1234))}, mc.cores=length(numberOfTopics))

# Calculate the main digitalization measure
refDoc<-cleanCorpus(refDoc)   # preprocess the textual data in the reference document on digitalization
dtmRefDoc<-DocumentTermMatrix(refDoc)
klDivergenceList<-mclapply(as.list(1:length(ldaList)), function(k){calculateKlDivergence(lda=ldaList[[k]], dtmDig=dtmRefDoc)},mc.cores=length(ldaList))

# Provide the digitalization measures in a dataframe
digitalizationMeasure<-formatMeasure(klList=klDivergenceList,colNames=paste0(seq(15,60,by=15),"_topics"))