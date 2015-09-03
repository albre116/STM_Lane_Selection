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
if(!require("ggplot2"))
  install.packages("ggplot2")

REFIT <- FALSE  ###refit the model?

if(exists("data/stmFit.RData") | !REFIT){
  load("data/stmFit.RData")
  }else{

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
save(stmFit,SelectCorpus,out,RAW,file="data/stmFit.RData")
}####end else wrapper for quick load

########################
###Assign a New Load to a topic
###For a given Carrier (i.e. we need to derive the [topic|word,document] distribution)
###Using equation 27.37 in Murphy, Machine Learning a Probabilistic Perspective
########################
load <- "ca-nj" ###here is the target we want to get a topic classification for
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

#############################################
####establish carrier customer topic distance
####for the load of interest
####We start with cosine distance to take into account
####the raw number of shipments and the topic alignment
####Basically, we want a E(shipments) that align with topic
####Assignment
####Other distance measures may suffice too
#############################################
CarrierTID <- grep("T",out$meta$ID)
postdraw <- model$theta[CarrierTID,]
unitvec_scale <- apply(postdraw,1,function(b){
  1/sqrt(sum(b^2))
})
wct <- lapply(CarrierTID,function(b){
  sum(documents[[b]][2,])
})
wct <- unlist(wct) ###get word count for carrier
PMF_unitvec <- (1/sqrt(sum(PMF^2)))*PMF
cosdist <- function(x, y) {x%*%y}
DistanceKernel <- function(c){
  x <- matrix(c(c,PMF_unitvec),nrow=2,byrow = T)
  dist.mat <- proxy::dist(x = x, method = cosdist)
  return(dist.mat)
}

###run similarity for load topic distribution versus 
###carrier topic distributions 1 at a time
scale_postdraw <- postdraw*unitvec_scale ###make unit vector length for cosine direction
similarity_angle <- apply(scale_postdraw,1,DistanceKernel) ###this is the amount of the dist that projects to the PMF
Ect <- similarity_angle*wct ###weight by shipments to get E(ct) of carrier in Load Corridor Distribution
relevence_lambda <- 1  ###important tuning parameter to be set [0,1] that governs matching similarity
Ect_margin <- sum(Ect)/sum(wct) ###how common is this projection in the data

####essentially this is number of shipments in lane by carrier vs lift (similar to FREX score)
x=log(Ect)
y=log(similarity_angle/Ect_margin)
relevance <- relevence_lambda*x + (1 - relevence_lambda)*y

plot(y~x,ylab="Log(lift)",xlab="log(Ect)")

ordered_post <- postdraw[order(relevance,decreasing = T),]
ordered_meta <- out$meta[CarrierTID,]
ordered_meta <- ordered_meta[order(relevance,decreasing = T),]
ordered_angle <- similarity_angle[order(relevance,decreasing = T)]
ordered_scale_postdraw <- scale_postdraw[order(relevance,decreasing = T),]
ordered_relevance <- relevance[order(relevance,decreasing = T)]
ordered_x <- x[order(relevance,decreasing = T)]
ordered_y <- y[order(relevance,decreasing = T)]

round(ordered_post[1,],2)
round(PMF,2)
sum(ordered_post[1,])
ordered_angle[1:100]


########################
###Investigate the Result
###By looking at carrier recommendations
########################
relevance_ecdf <- ecdf(ordered_relevance)
cutoff <- 0.01 ##keep cutoff or more
loss <- quantile(relevance_ecdf,1-cutoff) #give me the quantile for the cutoff percentile
hist(relevance,breaks=200,
     xlab="Relevance Score",
     main=paste0(round(cutoff,2)*100,
                 "% of Carriers in Terms of Relevance (L*log(Ect) + (1-L)*log(lift)): L=",
                 relevence_lambda))
abline(v=loss,lty=2)


####Now we strip out the relevant data from the lane distribution
ShortList <- ordered_meta[ordered_relevance>=loss,]
ShortList$relevance <- ordered_relevance[ordered_relevance>=loss]
ShortList$Log_Ect <- ordered_x[ordered_relevance>=loss]
ShortList$Log_Lift <- ordered_y[ordered_relevance>=loss]
ShortList$Lambda <- relevence_lambda
ShortList$Pct_Alignment <- exp(ShortList$Log_Ect)/ShortList$TransactionCount

p <- ggplot(ShortList,aes(x=Log_Ect,y=Log_Lift))+geom_point()
print(p)



#######################
####
####



###lets plot the topic divergence/similarity between carriers & load
###on a 2D grid using multi dimensional scaling
###for this we need to scale the relevance score
###to be a similarity measure

MDSScale <- function (phi){
  cosdist <- function(x, y) {x%*%y}
  dist.mat <- proxy::dist(x = phi, method = cosdist)
  pca.fit <- cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[, 1], y = pca.fit[, 2])
}

###add on the current load 1st row to the distance matrix
ID <- relevance>=loss
phi <- rbind(PMF_unitvec,ordered_scale_postdraw[ID,])
metaMDS <- ordered_meta[ID,]
###kernel distance matrix MDS projections in 2D for plotting
Kxx <- MDSScale(phi)

Kxx$Category <- "Carrier"
Kxx$Category[1] <- "Load"
Kxx$MeanNetRevenue <- NA
Kxx$MeanNetRevenue[-1] <- metaMDS$MeanNetRevenue
Kxx$MeanNetRevenue[1] <- max(metaMDS$MeanNetRevenue)

p <- ggplot(Kxx,aes(x=x,y=y,colour=Category,size=MeanNetRevenue))+geom_point()
print(p)




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





