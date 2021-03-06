if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(MplusAutomation,tidyverse,plyr)


#Function to extract p-value

#p value provided is used to assess if
#there is significant improvement between the specified model and a model with
#one less class. Looking at these p values, we identified the model selected based
#on the occurrence of the first nonsignificant p value (p > .05).

#Function to obtain position given satisfied condition 
pos <- function(x){
  min(which(x >= 0.05))+1
}

#Clean table. Extract pvalue for aVLMR and BLRT 
obtain_P <- function(Population,Split,Quality,Effect,Sample,Cov,LRT){
  target = file.path("/home/luos5",Population,Split,Quality,Effect,Sample,Cov,"Inputs")
  allOuts <- readModels(target, what = "summaries")
  allOuts_New <- do.call("rbind.fill",sapply(allOuts,"[","summaries"))
df_pval <- (allOuts_New %>% select(Filename,LRT)
              %>% separate(Filename,c("Class",NA,"Sample","Rep",NA,NA))
              %>% spread(Class,LRT))
  

#Define Index
size = dim(df_pval)[1]
from=3
to=dim(df_pval)[2]
#Storage
l <- data.frame()
lClass <- c()
  for (i in 1:size){
    for(j in from:to){
        l[i,j-2]<-df_pval[i,j] > 0.05
    }
    #add 1 to correct for index (labelling latent classes)
    lClass[i] <- min(which(l[i,]==TRUE)) + 1
  }
  df1 <- (data.frame(df_pval,lClass) %>% 
            dplyr::group_by(lClass) %>% dplyr::summarise(total=dplyr::n()/size))
  dff <- list(df_pval,df1)
return(dff)
}


obtainLMR <- function(pop,split,quality,effect,cov){
  storage <-list()
  sample = list(200,500,1000,2000)
    for (i in 1:length(sample)){
      storage[i]<-obtain_P(pop,split,quality,effect,sample[i],cov,"T11_LMR_PValue")[2]
    }
  return(storage)
}

obtainBLRT<- function(pop,split,quality,effect,cov){
  storage <-list()
  sample = list(200,500,1000,2000)
  for (i in 1:length(sample)){
    storage[i]<-obtain_P(pop,split,quality,effect,sample[i],cov,"BLRT_PValue")[2]
  }
  return(storage)
}


# #extract BLRT and aLMR p-value from data 
# pdta <- function(dta){
#   LRT <- list()
#   LRT[["VLMR"]] <-(dta
#                    %>% select(Filename,T11_VLMR_PValue)
#                    %>% separate(Filename,c("NumLC",NA,"SampleSize","nRep",NA))
#                    %>% mutate(nRep=as.numeric(nRep))
#                    %>% pivot_wider(names_from = NumLC,values_from=T11_VLMR_PValue))
  
#   LRT[["BLRT"]] <-(dta
#                    %>% select(Filename,BLRT_PValue)
#                    %>% separate(Filename,c("NumLC",NA,"SampleSize","nRep",NA))
#                    %>% mutate(nRep=as.numeric(nRep))
#                    %>% pivot_wider(names_from = NumLC,values_from=BLRT_PValue))
#   return(LRT)
# }



