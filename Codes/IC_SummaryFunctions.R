#Calculate - add to summary function 
#ICL = BIC + 2[(Ek-1)nlogK]
#CLC = -2LL + 2[(Ek-1)nlogK]

#setwd("/Users/sherry/Desktop")

#Testing data - comment out later 
#load("~/Desktop/200_sampleHQS104.Rda") #all_dta200
#load("~/Desktop/500_sampleHQS104.Rda") #all_dta500
#t <- list(all_dta200,all_dta500)

library(tidyverse)
library(stringr)
library(MplusAutomation)
library(plyr)

#Function returns table of selected IC of all replications values along with the selected latent class as determined by each IC value
tbl_idxSelector <- function(df,IC){
	(df #data frame with all IC values for every rep and latent class run (called data fit_tab)
	 %>% select(starts_with(IC))
	 %>% mutate(LC=apply(.,1,which.min)+1))}

#Funciton to return proportions that each index selected each LC 
tbl_prop <- function(tab_sample){
	reps <- dim(tab_sample)[1]
	LC2 <- sum(tab_sample$LC == 2)
	LC3 <- sum(tab_sample$LC == 3)	
	LC4 <- sum(tab_sample$LC == 4)	
	LC5 <- sum(tab_sample$LC == 5)		
	prop_vect <- round((c(LC2,LC3,LC4,LC5)/reps) * 100,2)
	#round(table(tab[,5])/reps,3)*100 #obtain percentages for each selected LC
return(prop_vect)
}

#Function returns matrix with proportions of a particular IC

IC_summary_fctn <- function(df_sample,IC){
	summary_table <- (df_sample  
		%>% select(Title, Filename,Parameters,LL, Entropy, AIC, BIC, aBIC)
		%>% separate(col=Filename, into=c("LClass","Model","Sample","Rep"))
		%>% mutate(Title=as_factor(Title), LClass=as.integer(LClass),Sample=as.integer(Sample),Rep=as_factor(Rep))
		%>% mutate(ICL=BIC + 2*((Entropy-1)*Sample*log(LClass)),
			       CLC=-2*LL + 2*((Entropy-1)*Sample*log(LClass)), CAIC=-2*LL + Parameters*log(Sample))
		 %>% select(Rep, Title, AIC, CAIC, BIC, aBIC, ICL,CLC)
		 %>% pivot_wider(names_from=Title, values_from= c(AIC,CAIC,BIC,aBIC,ICL,CLC)))
	
	summary_table_with_LC <- tbl_idxSelector(summary_table,IC)
	prop_vector <- tbl_prop(summary_table_with_LC)

	mat <-matrix(prop_vector,nrow=1,ncol=4, dimnames=list(c(IC),c("LC2","LC3","LC4","LC5")))
return(mat) 
}

obtain_IC_prop<- function(p,s,q,e,c){
	#p: population
	#s: split
	#q: quality 
	#e: effect 
	sample=list(200,500,1000,2000)
	all_dta_store <-list[[]]
	#c: no cov/with cov 
	for (i in 1:length(sample)){
		fp <- file.path("/home/luos5",p,s,q,e,sample[[i]],c,"Inputs")
		all_dta <- readModels(target = fp, what="summaries")
		all_dta_store[[i]] <- as_tibble(do.call("rbind.fill", sapply(all_dta,"[","summaries")))}
	#list for storage and ICs used
	list_IC <- list("AIC","CAIC","BIC","aBIC","ICL","CLC")
	list_df_samples <- all_dta_store
	store_IC <- list()
	list_store_IC_samples <- list()
	#loop through each IC and store 
	for (s in seq_along(list_df_samples)){
		for (ic in seq_along(list_IC)){
			store_IC[[ic]] <-IC_summary_fctn(list_df_samples,list_IC[[ic]])
		}
		list_store_IC_samples[[s]] <- store_IC
	}
	#store and rbind
	all <- list(do.call(rbind,list_store_IC_samples[[1]]), do.call(rbind,list_store_IC_samples[[2]]),
		do.call(rbind,list_store_IC_samples[[3]]), do.call(rbind,list_store_IC_samples[[4]]))
names(all) <- c("200","500","1000","2000") #name list called all 
return(all)
}

# #Testing Function 
# list_IC <- list("AIC","CAIC","BIC","aBIC","ICL","CLC")
# #list_df_samples <- getOutFiles()
# list_df_samples <- t
# store_IC <- list()
# list_store_IC_samples <- list()

# for (s in seq_along(list_df_samples)){
# 	for (ic in seq_along(list_IC)){
# 		store_IC[[ic]] <-IC_summary_fctn(list_df_samples,list_IC[[ic]])
# 	}
# 	list_store_IC_samples[[s]] <- store_IC
# }

# all <- list(do.call(rbind,list_store_IC_samples[[1]]), do.call(rbind,list_store_IC_samples[[2]]))
# names(all) <- c("200","500")
# print(all)


### Sample Output ###
#> all
#$`200`
#       LC2  LC3  LC4  LC5
# AIC  10.0 59.4 25.4  5.2
# CAIC 98.6  1.4  0.0  0.0
# BIC  98.6  1.4  0.0  0.0
# aBIC 14.2 64.6 18.6  2.6
# ICL   7.8 61.6 21.8  8.8
# CLC   0.0 10.0 21.2 68.8

# $`500`
#       LC2  LC3  LC4  LC5
# AIC  10.0 59.4 25.4  5.2
# CAIC 98.6  1.4  0.0  0.0
# BIC  98.6  1.4  0.0  0.0
# aBIC 14.2 64.6 18.6  2.6
# ICL   7.8 61.6 21.8  8.8
# CLC   0.0 10.0 21.2 68.8

### End Sample Ouput ### 










