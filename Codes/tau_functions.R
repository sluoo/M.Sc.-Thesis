#Finding thresholds for conditional-item response probabilities; 
library(tidyverse)
library(readxl)

probs <- citem
df <- as.tibble(probs)

#Condition Probabilities 
cond1 <- (df %>% select(Items,contains("-1")))
cond2 <- (df %>% select(Items,contains("-2")))
cond3 <- (df %>% select(Items,contains("-3")))

thresConv <- function(col){
  log((1/col)-1)
}

thres1 <- cbind(cond1[,1],apply(cond1[-1],2,thresConv))
thres2 <- cbind(cond2[,1],apply(cond2[-1],2,thresConv))
thres3 <- cbind(cond3[,1],apply(cond3[-1],2,thresConv))

thres1
thres2
thres3

#Thresholds for Mplus
cond1 <- cbind(cond1[,1],apply(cond1[-1],2,thresConv))
cond2 <- cbind(cond2[,1],apply(cond2[-1],2,thresConv))
cond3 <- cbind(cond3[,1],apply(cond3[-1],2,thresConv))

#Create special format for inputting thresholds for each 
#latent class
mplus_thres <- function(df){
  items <- rep("u",10)
  n <-seq(1,10)
  d <- rep("$",10)
  ones <- rep(1,10)
  star <-rep("*",10)
  l <- list()
  for (i in 1:3){
    p <- round(cond1[,i+1],2)
    y <- noquote(paste(items,n,d,ones,star,p, sep=""))
    l[[i]] <- y}
  return(l)}

