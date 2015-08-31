####################################
####################################
###Structural Topic Modeling Prototype
###Scripting File
####################################
####################################
rm(list=ls(all=TRUE))
gc()

if(!require("devtools"))
  install.packages("devtools")
if(!require("stm"))
  install.packages("stm")
if(!require("dplyr"))
  install.packages("dplyr")


#####Read in the data
#the variable of interest is RAW
load("data/2015_05_26.RData")

#####Make up the document corpus
#####By aggregating orig/dest pairs for each carrier (i.e. make a document with o/d info)
#####One document for each carrier T-code
RAW <- RAW %>% arrange(CarrierTCode)
RAW <- RAW %>% filter(CarrierTCode !="") #dump empties without carrier code (not many of them)
RAW <- RAW %>% mutate(OrigDestState = paste(OriginState,DestinationState,sep="-"))
#RAW <- RAW %>% mutate(OrigDestState = paste(OriginState,DestinationState,sep=","))

####Group loads in table by carrier
by_TCode <- RAW %>% group_by(CarrierTCode)

###create document corpus
Corpus <- by_TCode %>% summarise(TransactionCount = n(),
                                 MeanNetRevenue = mean(NetRevenue),
                                 OrigDocument = paste(OriginState,collapse=","),
                                 DestDocument = paste(DestinationState,collapse=","),
                                 Document = paste(OrigDestState,collapse=","))

###strip out commas and replace with spaces
Corpus <- Corpus %>% mutate(
  OrigDocument = gsub(","," ",OrigDocument),
  DestDocument = gsub(","," ",DestDocument),
  Document = gsub(","," ",Document))


###Plot histogram of Carrier Size
CarrierSize <- ecdf(Corpus$TransactionCount)
cutoff <- 100 ##keep cutoff or more
loss <- CarrierSize(cutoff-1) #Pr(X>=x)
hist(log(Corpus$TransactionCount),breaks=200,
     xlab="Log_e(Number of Carrier Transactions)",
     main=paste0("Carrier Transactions with Exclusion Line at X<",
                 cutoff,
                 " Removed"," (We exclude ",round(loss,2)*100,"% of Carriers)"))
abline(v=log(cutoff-0.5),lty=2)
SelectCorpus <- Corpus %>% filter(TransactionCount>=cutoff)
###add in shortened texts for display later on
nextract <- 50
shorttexts <- function(x){
  paste(na.omit(unlist(strsplit(x," "))[1:nextract]),collapse = " ")
}
SelectCorpus$ShortTexts <- unlist(lapply(SelectCorpus$Document,shorttexts))


###Begin a Topic Model for the Selected Corpus
processed <- textProcessor(SelectCorpus$Document,metadata = SelectCorpus,
                           lowercase = FALSE,removestopwords = F,removenumbers = F,
                           removepunctuation = F,stem = F,wordLengths = c(1,Inf))
out <- prepDocuments(processed$documents,processed$vocab,processed$meta,lower.thresh = 1)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
plotRemoved(processed$documents,lower.thresh = seq(1,200, by=5)) ###if we choose to eliminate

###now begin model construction
stmFit <- stm(out$documents,out$vocab,K=0,max.em.its = 75,data=meta,init.type = "Spectral")

########################
###look at model objects
toLDAvis(stmFit,out$documents)

########################
###Get topic classifications
###For individual Words
########################

draws <- thetaPosterior(stmFit,nsims=1)
draws[[1]]

dim(stmFit$theta)


###add in diagnostics later
#TBD

###Evaluate Fitted Model Results
labelTopics(stmFit)
plot.STM(stmFit,type="summary",n=5)

CarrierTopics <- findThoughts(stmFit,texts = SelectCorpus$ShortTexts,n=3)
###word cloud of cooridors
for(i in 1:length(CarrierTopics$docs)){
cloud(stmFit,topic=i)
title(names(CarrierTopics$docs)[i])
}

###exact carrier matches

for(i in 1:length(CarrierTopics$docs)){
  plotQuote(CarrierTopics$docs[[i]],main=names(CarrierTopics$docs)[i])
}

####topic correlation
corr.model <- topicCorr(stmFit)
plot.topicCorr(corr.model)

######
###add in covariate to distinguish by carrier size the topic distribution
######
stmFit <- stm(out$documents,out$vocab,K=0,max.em.its = 75,
              data=meta,init.type = "Spectral",prevalence =~s(TransactionCount))

###Evaluate Fitted Model Results
labelTopics(stmFit)
plot.STM(stmFit,type="summary",n=5)

CarrierTopics <- findThoughts(stmFit,texts = SelectCorpus$ShortTexts,n=3)
###word cloud of cooridors
for(i in 1:length(CarrierTopics$docs)){
  cloud(stmFit,topic=i)
  title(names(CarrierTopics$docs)[i])
}

###exact carrier matches

for(i in 1:length(CarrierTopics$docs)){
  plotQuote(CarrierTopics$docs[[i]],main=names(CarrierTopics$docs)[i])
}



###estimate transaction count effect
effect <- estimateEffect(1:5~s(TransactionCount),stmFit,meta=meta,uncertainty = "Global")
plot.estimateEffect(effect,covariate = "TransactionCount",ci.level = 0.95,
                    method="continuous",topics = c(3))

plot.estimateEffect(effect,covariate = "TransactionCount",ci.level = 0.95,
                    method="pointestimate",topics = c(1))

####topic correlation
corr.model <- topicCorr(stmFit)
plot.topicCorr(corr.model)

#########################################
#####Pull in other visulization packages
#####From the D3 js environment
#########################################

if(!require("stmBrowser"))
  install_github("mroberts/stmBrowser",dependencies=TRUE)

#setwd(tempdir())###move to a float environment that will be cleared on exit
stmBrowser(stmFit,data=meta,covariates = c("TransactionCount"),text = "Document")


####next package
if(!require("stmCorrViz"))
  install.packages("stmCorrViz")

stmCorrViz(stmFit,"corrviz.html",
           documents_raw = SelectCorpus$Document)





