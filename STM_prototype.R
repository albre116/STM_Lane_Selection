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

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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
RAW <- RAW %>% arrange(CarrierTCode)
RAW <- RAW %>% mutate(OrigDestState = tolower(paste(OriginState,DestinationState,sep="-")))
save(stmFit,SelectCorpus,out,RAW,file="data/stmFit.RData")
}####end else wrapper for quick load

########################
###Assign a New Load to a topic
###For a given Carrier (i.e. we need to derive the [topic|word,document] distribution)
###Using equation 27.37 in Murphy, Machine Learning a Probabilistic Perspective
########################

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

#########################
####### big function wrapper here
#########################
STMRecommender <- function(load="ca-ca",customer="C331640",percentile_cutoff=0.01,Show=5){

Load_PMF <- function(load,customer){
#load <- "ca-nj" ###here is the target we want to get a topic classification for
load_ID <- which(out$vocab == load)
#customer <- "C331640" ###here is the customer we want to assign a load to
customer_ID <- which(out$meta$ID == customer) ### get the document corpus ID

TopicAssignWordCarrier <- function(k){
###Part 1 of equation 27.37
#k is the topic dimension of interest (will be added to all)
Cvk <- Word_Topic_Matrix[k,load_ID]  #number of times word v is assigned to topic K across corpus
gamma <- 100 ###smoothing prior psuedo count in first term for global specific smoothing
V <- ncol(Word_Topic_Matrix) ###size of vocabulary 
Ck <- Topic_Margin[k]#number of words assigned to topic k

###Part 2 of equation 27.37
#####Now We need to estimate how often a topic is used in a given document
if(length(customer_ID)>0){  ###we find the customer in the register
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
}else{ ###we don't find the customer
  Cik <- 0  ##no counts found
  Li <- 0  ##no counts found
}

K <- length(Topic_Margin)  ###number of total topics
alpha <- 1 ###smoothing parameter for the document specific parameter

####Putting it all together
D_unormalized <- ((Cvk + gamma)/(Ck + V*gamma))*((Cik + alpha)/(Li + K*alpha))
return(D_unormalized)
}

Density <- unlist(lapply(1:length(Topic_Margin),TopicAssignWordCarrier))
PMF <- Density/sum(Density)
names(PMF) <- paste("Topic",1:length(PMF))
#print(round(PMF,2))
return(list(PMF=PMF,Word_Margin=Word_Margin,Topic_Margin=Topic_Margin))
}###end function

PMF_estimate <- Load_PMF(load=load,customer=customer)  ###pull this down
PMF <- PMF_estimate[["PMF"]]


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

#plot(y~x,ylab="Log(lift)",xlab="log(Ect)")


####order by relevance

ordered_post <- postdraw[order(relevance,decreasing = T),]
ordered_meta <- out$meta[CarrierTID,]
ordered_meta <- ordered_meta[order(relevance,decreasing = T),]
ordered_CarrierTID <- CarrierTID[order(relevance,decreasing = T)]
ordered_angle <- similarity_angle[order(relevance,decreasing = T)]
ordered_scale_postdraw <- scale_postdraw[order(relevance,decreasing = T),]
ordered_relevance <- relevance[order(relevance,decreasing = T)]
ordered_x <- x[order(relevance,decreasing = T)]
ordered_y <- y[order(relevance,decreasing = T)]

#round(ordered_post[1,],2)
#round(PMF,2)
#sum(ordered_post[1,])
#ordered_angle[1:100]


########################
###Investigate the Result
###By looking at carrier recommendations
########################
relevance_ecdf <- ecdf(ordered_relevance)
cutoff <- percentile_cutoff ##keep cutoff or more
loss <- quantile(relevance_ecdf,1-cutoff) #give me the quantile for the cutoff percentile
# hist(relevance,breaks=200,
#      xlab="Relevance Score",
#      main=paste0(round(cutoff,2)*100,
#                  "% of Carriers in Terms of Relevance (L*log(Ect) + (1-L)*log(lift)): L=",
#                  relevence_lambda))
# abline(v=loss,lty=2)


####Now we strip out the relevant data from the lane distribution
CarrierTID_cut <- ordered_CarrierTID[ordered_relevance>=loss]
ShortList <- ordered_meta[ordered_relevance>=loss,]
ShortList$relevance <- ordered_relevance[ordered_relevance>=loss]
ShortList$Log_Ect <- ordered_x[ordered_relevance>=loss]
ShortList$Log_Lift <- ordered_y[ordered_relevance>=loss]
ShortList$Lambda <- relevence_lambda
ShortList$Pct_Alignment <- exp(ShortList$Log_Ect)/ShortList$TransactionCount
ShortList$Ect <- exp(ShortList$Log_Ect)

#p <- ggplot(ShortList,aes(x=Log_Ect,y=Log_Lift))+geom_point()
#print(p)



###########################################################################
###Now we need to put estimate the topic distribution for each carrier load
###So we can compute expected net revenue for the projection
###Intersecting the customer load PMF topic distribution
###########################################################################

###we will loop over all i
CarrierAlignment <- lapply(CarrierTID_cut,function(i){
  #print(i)
  customer_ID <- i ### get the document corpus ID
  doc.ct <- documents[[customer_ID]][2,]
  doc.vocab <- documents[[customer_ID]][1,]
  eta <- lambda[customer_ID,]
  theta <- model$theta[customer_ID,]
  #get just the necessary columns of beta
  doc.beta <-   beta[[betaindex[customer_ID]]][,documents[[customer_ID]][1,],drop=F]
  expeta <- c(exp(eta),1)
  EB <- expeta*doc.beta #calculate exp(eta)\beta for each word
  EB <- t(EB)/colSums(EB) #transpose and norm by (now) the row
  phi <- EB*(doc.ct) #multiply through by word count
  Li <- sum(doc.ct)  ###total number of words in target document
  K <- length(Topic_Margin)  ###number of total topics
  alpha <- 1 ###smoothing parameter for the document specific parameter
  gamma <- 100 ###smoothing prior psuedo count in first term for global specific smoothing
  #now we need to loop over all j for the vocabulary of that CarrierTID
  
  projection <- lapply(1:length(doc.vocab),function(j){
    v.ct <- doc.ct[j]
    load_ID <- doc.vocab[j]
    ###this is an apply function that will be run over k inside the loop
    TopicAssignWordCarrier <- function(k){
      ###Part 1 of equation 27.37
      #k is the topic dimension of interest (will be added to all)
      Cvk <- Word_Topic_Matrix[k,load_ID] #number of times word v is assigned to topic K across corpus
      V <- ncol(Word_Topic_Matrix) ###size of vocabulary 
      Ck <- Topic_Margin[k]#number of words assigned to topic k
      ###Part 2 of equation 27.37
      #####Now We need to estimate how often a topic is used in a given document
      Cik <- colSums(phi)[k]  ###how often Topic k is used in target document
      ####Putting it all together
      D_unormalized <- ((Cvk + gamma)/(Ck + V*gamma))*((Cik + alpha)/(Li + K*alpha))
      return(D_unormalized)
    }
    
    Density <- unlist(lapply(1:length(Topic_Margin),TopicAssignWordCarrier))
    PMF_CCode_v <- Density/sum(Density)
    
    ###get angular projection of this load to topic vector of interest
    PMF_CCode_v_unitvec <- as.matrix((1/sqrt(sum(PMF_CCode_v^2)))*PMF_CCode_v,nrow=1)
    projection <- PMF_unitvec %*% PMF_CCode_v_unitvec
    return(projection)
  })
  
  doc.alignment <- data.frame(OrigDestState=unlist(out$vocab[doc.vocab]),
                              VocabID=as.numeric(doc.vocab),
                              DocCount=as.numeric(doc.ct),
                              Projection=as.numeric(projection),stringsAsFactors = F)###summary statistics for topic alignment of each word
  return(doc.alignment)
})###close outer loop

#CarrierAlignment[[1]]
#out$vocab


####loop over each carrier in the selected data (i)

ProjNetRev <- lapply(1:length(CarrierTID_cut),function(i){
pick <- CarrierTID_cut[i]
Tcode <- out$meta$ID[pick]
###do select operation to parse down
TMP <- RAW %>% 
  filter(CarrierTCode==Tcode) %>% 
  select(NetRevenue,OrigDestState)

###now join on projection data by vocab

TMP <- TMP %>% left_join(CarrierAlignment[[i]])

###get rid of the zeros (they are non-informative and a big chunk of the data (sad... check with tim))
TMP$NetRevenue[TMP$NetRevenue==0] <- NA

###Do the projection and average by the projection weights
objective <- TMP$NetRevenue*TMP$Projection
keep <- !is.na(objective)
ProjNetRev <- sum(objective[keep])/sum(TMP$Projection[keep])
return(ProjNetRev)
})

ProjNetRev <- unlist(ProjNetRev)  ###here is the net revenue aligned with that topic assignment for the load

####Make the Plot
ShortList$ProjNetRev <- ProjNetRev
# p <- ggplot(ShortList,aes(x=Log_Ect,y=Log_Lift,size=ProjNetRev))+geom_point()
# print(p)


####Ok now lets get together a demo here
standardize <- function(x){
  minx <- min(x)
  maxx <- max(x)
  x <- x-minx
  x <- x/(maxx-minx)
  return(x)
}
ShortList$EctUnit <- standardize(ShortList$Log_Ect)
ShortList$LiftUnit <- standardize(ShortList$Log_Lift)
ShortList$ProjNetRevUnit <- standardize(ShortList$ProjNetRev)
ShortList$relevance <- (1/3)*ShortList$EctUnit +
  (1/3)*ShortList$LiftUnit +
  (1/3)*ShortList$ProjNetRevUnit

Final <- ShortList %>% arrange(desc(relevance))
order <- c("ID", 
           "relevance",
           "ProjNetRev",
           "MeanNetRevenue",
           "Ect",
           "EctUnit",
           "LiftUnit",
           "ProjNetRevUnit",
           "Document")
Final <- Final[,order]
Final$Ranking <- 1:nrow(Final)
Final$Label <- paste(Final$Ranking,Final$ID,sep=":")

Final$Top <- "blue"
Final$Top[1:Show] <- "red"


####Make the Plot
p1 <- ggplot(Final,aes(x=EctUnit,y=LiftUnit,size=relevance,colour = Top))+geom_point()
p1 <- p1 + geom_text(data=Final[1:Show,],aes(x=EctUnit,y=LiftUnit,label = Label),colour = "black",hjust=-0.25)
p1 <- p1 + ggtitle(paste("Scaled Shipping Volume vs Lift with Top", Show,"most Relevant Identified")) + 
  theme(legend.position="none") +
  xlab("Scaled Expected Volume in Load Corridor") + 
  ylab("Scaled Lift (relevance to Load Corridor)")


p2 <- ggplot(Final,aes(x=EctUnit,y=ProjNetRevUnit,size=relevance,colour = Top))+geom_point()
p2 <- p2 + geom_text(data=Final[1:Show,],aes(x=EctUnit,y=ProjNetRevUnit,label = Label),colour = "black",hjust=-0.25)
p2 <- p2 + ggtitle(paste("Scaled Shipping Volume vs Projected Net Revenue with Top", Show,"most Relevant Identified")) + 
  theme(legend.position="none") +
  xlab("Scaled Expected Volume in Load Corridor") + 
  ylab("Scaled Projected Net Revenue (specific to Load Corridor)")

multiplot(p1,p2)
return(list(PMF=PMF,Final=Final))
}###end big wrapper


#############################################################
#############################################################
#############################################################
####Model Demo Code Right Here
#############################################################
#############################################################
#############################################################

#####Investigate Topics and Their Definitions


###Evaluate Fitted Model Results
labelTopics(stmFit)
plot.STM(stmFit,type="summary",n=5)

CarrierTopics <- findThoughts(stmFit,texts = SelectCorpus$ShortTexts,n=3)
###word cloud of cooridors
for(i in 1:length(CarrierTopics$docs)){
  cloud(stmFit,topic=i)
  title(names(CarrierTopics$docs)[i])
}




###some good load and lane examples
### load="ca-nj",customer="C331640"
### load="ca-ca",customer="C331640"
### load="az-co",customer="C989016"



ClassifierResult <- STMRecommender(load="ca-nj",customer="C331640",percentile_cutoff=0.01,Show=5)
plotdata <- ClassifierResult[["PMF"]]
names(plotdata) <- 1:length(plotdata)
print(round(plotdata,2))
barplot(plotdata,horiz=T,xlab="Probability",ylab="Corridor (Topic)")
BestProspects <- ClassifierResult[["Final"]]

check <- order(plotdata,decreasing=T)
investigate <- 1

for(i in check[1:investigate]){
  cloud(stmFit,topic=i)
  title(names(CarrierTopics$docs)[i])
}





##############################################
##############################################
####End of Demo
##############################################
##############################################





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





