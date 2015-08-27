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


####Group loads in table by carrier
by_TCode <- RAW %>% group_by(CarrierTCode)

###create document corpus
Corpus <- by_TCode %>% summarise(TransactionCount = n(),
                                 OrigDocument = paste(OrigCity,collapse=","),
                                 DestDocument = paste(DestCity,collapse=","),
                                 Document = paste(OrigCity,DestCity,collapse=","))

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


###Begin a Topic Model for the Selected Corpus
processed <- textProcessor(SelectCorpus$Document,metadata = SelectCorpus[,"TransactionCount",drop=F])
out <- prepDocuments(processed$documents,processed$vocab,processed$meta,lower.thresh = 1)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
plotRemoved(processed$documents,lower.thresh = seq(1,200, by=5)) ###if we choose to eliminate

###now begin model construction
stmFit <- stm(out$documents,out$vocab,K=20,max.em.its = 75,data=meta,seed=112)







