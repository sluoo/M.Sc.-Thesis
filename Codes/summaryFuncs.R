if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(MplusAutomation,tidyverse,plyr)


#Function to extract p-value

#p value provided is used to assess if
#there is significant improvement between the specified model and a model with
#one less class. Looking at these p values, we identified the model selected based
#on the occurrence of the first nonsignificant p value (p > .05).


pos <- function(x){
  min(which(x >= 0.05))+1
}

pdta <- function(dta){
  LRT <- list()
  LRT[["VLMR"]] <-(dta
                   %>% select(Filename,T11_VLMR_PValue)
                   %>% separate(Filename,c("NumLC",NA,"SampleSize","nRep",NA))
                   %>% mutate(nRep=as.numeric(nRep))
                   %>% pivot_wider(names_from = NumLC,values_from=T11_VLMR_PValue))
  
  LRT[["BLRT"]] <-(dta
                   %>% select(Filename,BLRT_PValue)
                   %>% separate(Filename,c("NumLC",NA,"SampleSize","nRep",NA))
                   %>% mutate(nRep=as.numeric(nRep))
                   %>% pivot_wider(names_from = NumLC,values_from=BLRT_PValue))
  return(LRT)
}


summary <- function(population,split,criteria=c("pvalue","IC")){
  #construct file path
  #quality <- c("HighQuality","ModerateQuality","LowQuality")
  #effect <- c('0.4','0.9','1.5')
  #sample <- c('200','500','1000','2000')
  #cov <- c("NoCov","WithCov")
  
  quality <- c("HighQuality")
  effect <- c('0.4')
  sample <- c('200')
  cov <- c("NoCov",'WithCov')
  main <-list()
  
    for (q in quality){
      for(e in effect){
        for (s in sample){
          for (c in cov){
            
            #fp <- file.path("/home/luos5",population,split,q,e,s,c,"Inputs")
            fp_test <- file.path("/home/luos5/test1/NoCov/Inputs") #delete later
            allOut <- readModels(fp_test, what= "summaries")
            allOutBind <- as_tibble(do.call("rbind.fill", sapply(allOut,"[","summaries")))
            
            if (criteria == "pvalue"){
              dta_v <- pdta(allOutBind)$VLMR
              l <- dim(pdta(dta)$VLMR)[1]
              v_LC <- (dta_v 
                           %>% select(-c(SampleSize,nRep))
                           %>% apply(1,pos)) 
              return(cbind(dta_v,v_LC))
              
              

              
              
              
              
              
              
            }
            else{
              
            
            }
            
            
            
            
    
          }
        }
      }
    }
}







#Function to extract IC + entropy 




