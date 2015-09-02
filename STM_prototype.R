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

START <- RAW

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
cutoff <- 10 ##keep cutoff or more
loss <- CarrierSize(cutoff-1) #Pr(X>=x)
hist(log(Corpus$TransactionCount),breaks=200,
     xlab="Log_e(Number of Carrier Transactions)",
     main=paste0("Carrier Transactions with Exclusion Line at X<",
                 cutoff,
                 " Removed"," (We exclude ",round(loss,2)*100,"% of Carriers)"))
abline(v=log(cutoff-0.5),lty=2)
SelectCorpus_Carrier <- Corpus %>% filter(TransactionCount>=cutoff)
colnames(SelectCorpus_Carrier)[1] <- "ID"


#####Do the same exercise for the customer data

#####Make up the document corpus
#####By aggregating orig/dest pairs for each carrier (i.e. make a document with o/d info)
#####One document for each carrier T-code
RAW <- START
RAW <- RAW %>% arrange(CustomerCCode)
RAW <- RAW %>% filter(CustomerCCode !="") #dump empties without carrier code (not many of them)
RAW <- RAW %>% mutate(OrigDestState = paste(OriginState,DestinationState,sep="-"))
#RAW <- RAW %>% mutate(OrigDestState = paste(OriginState,DestinationState,sep=","))

####Group loads in table by carrier
by_CCode <- RAW %>% group_by(CustomerCCode)

###create document corpus
Corpus <- by_CCode %>% summarise(TransactionCount = n(),
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
cutoff <- 10 ##keep cutoff or more
loss <- CarrierSize(cutoff-1) #Pr(X>=x)
hist(log(Corpus$TransactionCount),breaks=200,
     xlab="Log_e(Number of Carrier Transactions)",
     main=paste0("Carrier Transactions with Exclusion Line at X<",
                 cutoff,
                 " Removed"," (We exclude ",round(loss,2)*100,"% of Carriers)"))
abline(v=log(cutoff-0.5),lty=2)
SelectCorpus_Customer <- Corpus %>% filter(TransactionCount>=cutoff)
colnames(SelectCorpus_Customer)[1] <- "ID"
SelectCorpus <- bind_rows(SelectCorpus_Carrier,SelectCorpus_Customer)
rm(RAW,START,SelectCorpus_Carrier,SelectCorpus_Customer,by_CCode,by_TCode,Corpus,loss,cutoff)

###Begin a Topic Model for the Selected Corpus
processed <- textProcessor(SelectCorpus$Document,metadata = SelectCorpus,
                           lowercase = FALSE,removestopwords = F,removenumbers = F,
                           removepunctuation = F,stem = F,wordLengths = c(1,Inf))
out <- prepDocuments(processed$documents,processed$vocab,processed$meta,lower.thresh = 1)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
plotRemoved(processed$documents,lower.thresh = seq(1,200, by=5)) ###if we choose to eliminate

###add in shortened texts for display later on
nextract <- 50
shorttexts <- function(x){
  paste(na.omit(unlist(strsplit(x," "))[1:nextract]),collapse = " ")
}
SelectCorpus$ShortTexts <- unlist(lapply(SelectCorpus$Document,shorttexts))

###now begin model construction
stmFit <- stm(out$documents,out$vocab,K=0,max.em.its = 75,data=meta,init.type = "Spectral")


########################
###Assign a New Load to a topic
###For a given Carrier (i.e. we need to derive the [topic|word,document] distribution)
###Using equation 27.37 in Murphy, Machine Learning a Probabilistic Perspective
########################
load <- "ca-tn" ###here is the target we want to get a topic classification for
load_ID <- which(out$vocab == load)
customer <- "C331640" ###here is the customer we want to assign a load to
customer_ID <- which(out$meta == customer) ### get the document corpus ID

#define some parameters
model <- stmFit
sigma <- model$sigma
siginv <- model$invsigma
mu <- model$mu$mu
lambda <- model$eta
beta <- lapply(model$beta$logbeta, exp)
betaindex <- model$settings$covariates$betaindex
documents <- out$documents

#####create a count of specific words by topic matrix for the document
#####specific topic deviations EB = exp(lambda)*global.beta
Word_Topic_Matrix <- matrix(0,nrow=nrow(beta[[1]]),ncol=ncol(beta[[1]]))
for (i in 1:nrow(lambda)){
  doc.ct <- documents[[i]][2,]
  eta <- lambda[i,]
  theta <- model$theta[i,]
  #get just the necessary columns of beta
  doc.beta <-   beta[[betaindex[i]]][,documents[[i]][1,],drop=F]
  expeta <- c(exp(eta),1)
  EB <- expeta*doc.beta #calculate exp(eta)\beta for each word
  EB <- t(EB)/colSums(EB) #transpose and norm by (now) the row
  phi <- EB*(doc.ct) #multiply through by word count
  Word_Topic_Matrix[,documents[[i]][1,]] <- Word_Topic_Matrix[,documents[[i]][1,]] + t(phi)
}

###Generate Margins of the Word_Topic_Matrix
Word_Margin <- colSums(Word_Topic_Matrix)
Topic_Margin <- rowSums(Word_Topic_Matrix)


TopicAssignWordCarrier <- function(k){
###Part 1 of equation 27.37
#k is the topic dimension of interest (will be added to all)
Cvk <- Word_Topic_Matrix[k,load_ID]  #number of times word v is assigned to topic K across corpus
gamma <- 100 ###smoothing prior psuedo count in first term for global specific smoothing
V <- ncol(Word_Topic_Matrix) ###size of vocabulary 
Ck <- Topic_Margin[k]#number of words assigned to topic k

###Part 2 of equation 27.37
#####Now We need to estimate how often a topic is used in a given document

doc.ct <- documents[[customer_ID]][2,]
eta <- lambda[customer_ID,]
theta <- model$theta[customer_ID,]
#get just the necessary columns of beta
doc.beta <-   beta[[betaindex[customer_ID]]][,documents[[customer_ID]][1,],drop=F]
expeta <- c(exp(eta),1)
EB <- expeta*doc.beta #calculate exp(eta)\beta for each word
EB <- t(EB)/colSums(EB) #transpose and norm by (now) the row
phi <- EB*(doc.ct) #multiply through by word count
###part 2 equation terms
Cik <- colSums(phi)[k]  ###how often Topic k is used in target document 
Li <- sum(doc.ct)  ###total number of words in target document
K <- length(Topic_Margin)  ###number of total topics
alpha <- 1 ###smoothing parameter for the document specific parameter

####Putting it all together
D_unormalized <- ((Cvk + gamma)/(Ck + V*gamma))*((Cik + alpha)/(Li + K*alpha))
return(D_unormalized)
}

Density <- unlist(lapply(1:length(Topic_Margin),TopicAssignWordCarrier))
PMF <- Density/sum(Density)
names(PMF) <- paste("Topic",1:length(PMF))
print(round(PMF,2))


####establish carrier customer topic distance
####for the load of interest
CarrierTID <- grep("T",out$meta$ID)
postdraw <- model$theta[CarrierTID,]


DistanceKernel <- function(c){
  r <- PMF
  jensenShannon <- function(x, y) {
    m <- 0.5 * (x + y)
    0.5 * sum(x * log(x/m)) + 0.5 * sum(y * log(y/m))
  }
  x <- matrix(c(c,r),nrow=2,byrow = T)
  dist.mat <- proxy::dist(x = x, method = jensenShannon)
  return(dist.mat)
}

similarity <- apply(postdraw,1,DistanceKernel)

ordered_post <- postdraw[order(similarity),]
ordered_meta <- out$meta[CarrierTID,]
ordered_meta <- ordered_meta[order(similarity),]



########################
###look at model objects
###this function has some errors where it is scrambling things...
###It reorders the topic order that we use to something else and renames the topics
###the topics are all the same but different order
toLDAvis(stmFit,out$documents)

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





