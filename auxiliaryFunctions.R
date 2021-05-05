library(stringr)
library(qdapRegex)
library(tidytext)
library(dplyr)
library(philentropy)
library(tm)
library(readxl)



### Preprocessing the textual data ####

# Import stop words
  # We thank Bill McDonald for providing lists of common stopwords under the URL https://sraf.nd.edu/textual-analysis/resources/.
stopWords<-tolower(as.vector(read.delim(<filePathStopWordsTextFile>,header=FALSE)$V1))
stopWords<-str_trim(stopWords, side="both") 
stopWords<-stopWords[!(is.na(stopWords))]

# Preprocess the text corpus
cleanCorpus <- function(corp, stopwords=TRUE, stemming = TRUE) {
  # stopwords: If TRUE stopwords are removed.
  # stemming:  If TRUE stemming is performed.
  corp <- 
    tm_map(corp, content_transformer(function(x)
      iconv(x, to = 'UTF-8', sub = 'byte')))  # convert to UTF-8
  corp <-
    tm_map(corp, content_transformer(tolower)) # transform to lower-case
  corp <-
    tm_map(corp, removePunctuation)            # remove punctuation
  
  # clean data on a word basis
  for (k in 1:length(corp)) {
    corp[[k]][["content"]] <- cleanString(corp[[k]][["content"]])
  }
  
  if (stopwords) {
    # The stopwords have to be divided in multiple batches in order to be processed without causing errors.
    corp <- tm_map(corp, removeWords, stopWords[1:3500])
    corp <- tm_map(corp, removeWords, stopWords[3501:7000])
    corp <- tm_map(corp, removeWords, stopWords[7001:10500])
    corp <- tm_map(corp, removeWords, stopWords[10501:length(stopWords)])
  }
  if (stemming) {
    corp <- tm_map(corp, stemDocument, language = c("english"))
  }
  return(corp)
}

# auxiliary function for text preprocessing used in cleanCorpus()
cleanString<-function(string){
  string<-string[!(string=="")]
  string<-gsub("http[^[:space:]]*", "", string)   #remove URLs
  string<-gsub(pattern="[^\x01-\x7f]", replacement="",x=string) #remove non ASCII characters
  string<-str_replace_all(string, c("\f" = "", "\n"="", "\\d"=""))
  string<-str_replace_all(string, "[^[:alnum:]]", " ")  #remove all non alpha-numerical characters
  string<-str_replace_all(string, "[[:punct:]]", " ")  #remove punctuation characters
  # exclude error-messages from xpdf-reader:
  string<-string[!grepl("syntax",string)]
  string<-string[!grepl("error",string)]
  string<-string[!grepl("character",string)]
  string<-qdapRegex::rm_nchar_words(string, 1)  #remove single character words
  string<-removeNumbers(string)
  string<-string[!grepl("^$",string)]  #remove empty lines
  return(string)
}


### Calculate KL-divergence and the digitalization measure ####

# Calculates the KL divergence between the topic distributions in the annual reports and in the reference document on digitalization
calculateKlDivergence<-function(lda, dtmDig){
  # lda: fitted lda model 
  # dtmDig: Document Term Matrix of the reference document on digitalization
  digPerDocument<-rep(0,nrow(lda@gamma))
  names(digPerDocument)<-lda@documents
  posteriorDig<-posterior(lda,dtmDig)   
  
  for (k in 1:nrow(lda@gamma)){
    x<-rbind(posteriorDig$topics[1,],
             lda@gamma[k,])
    digPerDocument[k]=KL(x)
  }
  return(1/digPerDocument)
}


### Formatting the outputs ####

# Converts the results to a dataframe and scales values to the interval [0,1]
formatMeasure<-function(klList,colNames){
  if (length(colNames)!=length(klList)){stop("Length of colNames and length of klList differ.")}
  digitalizationMeasure<-NULL
  for (k in 1:length(klList)){
    # scale values to the interval [0,1]
    digitalizationMeasure<-cbind(digitalizationMeasure,scaleMinMax(klList[[k]]))
  }
  
  documentNames<-rownames(digitalizationMeasure)   # contains the names of the pdf-files. Structure in our sample: "ID_year.pdf"
  colnames(digitalizationMeasure)<- colNames
  ID<-as.numeric(unlist(str_split(documentNames,"_"))[(1:length(documentNames))*2-1])
  year<-as.numeric(substr(unlist(str_split(documentNames,"_"))[(1:length(documentNames))*2],start=1,stop=4))
  
  digitalizationMeasure<-data.frame(ID,year,digitalizationMeasure)
  return(digitalizationMeasure)
}

# auxiliary function for scaling values linearly to the interval [0,1]
scaleMinMax<-function(x){
  if(any(is.na(x))){warning("The vector contains NAs!")}
  if(max(x,na.rm=TRUE)==min(x,na.rm=TRUE)){stop("Scaling not possible because min = max!")
  } else {
    return(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))
  }
}