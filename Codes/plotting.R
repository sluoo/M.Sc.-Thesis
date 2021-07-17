library(tidyverse)
library(ggh4x)
library(readxl)
library(forcats)


##################
#
#
# Analysis 
#
##################


##### Direct Effect Vs. Split 
deVsSplit <- (read_excel("Desktop/Pop1 2.xlsx", sheet = "Direct Effect Vs Split")
                        %>% mutate(#Quality=recode(Qu, HighQuality="High Quality", ModerateQuality="Moderate Quality", LowQuality="Low Quality"),
                          DirectEffects=factor(DirectEffects,levels = c("0.4","0.9","1.5")),
                          Split=recode(Split, Split1="Split 1 - (60,35,5)",Split2="Split 2 - (45,40,15)"),
                          Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates"),
                          Population=recode(Population,Pop1="Population A", Pop2="Population B"),
                          Criteria=factor(Criteria,levels = c("aBIC","BLRT","BIC","aVLMR","CAIC","ICL","AIC","CLC")),
                          DirectEffects=recode(DirectEffects,"0.4"="Small Effect","0.9"="Medium Effect","1.5"="Large Effect")))

directEffect_Split <- print(ggplot(deVsSplit)
                      + geom_point(aes(x=Covariates,y=Averages, color=Criteria,shape=Criteria),size=3) 
                      + geom_line(aes(x=Covariates,y=Averages, color=Criteria,group=Criteria),size=1)
                      + facet_nested(DirectEffects ~ Population + Split,
                                     nest_line = TRUE,
                                     scale="free",
                                     switch="y",
                                     labeller = label_wrap_gen(width = 22,multi_line = TRUE))
                      + theme_bw()
                      + scale_shape_manual(values=c(15,20,17,18,19,16,15,3))
                      + scale_color_manual(values = c("#E41A1C","#FF7F00","#4DAF4A","#984EA3",
                                                      "#1F78B4","#A6CEE3","#B2DF8A","#33A02C"))
                      + theme(legend.position = "top",
                              #legend.justification = "right",
                              legend.background = element_rect(color = "grey90", fill = "grey90",size=0.1),
                              legend.text = element_text(size=11),
                              strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                              strip.text.y = element_text(size = 12),
                              legend.title = element_blank(),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              axis.text.x = element_text(size=11,angle=45,hjust=1),
                              axis.text.y = element_text(size=11))
                      + scale_y_continuous(breaks = seq(0, 90, by = 15))
                      + coord_cartesian(ylim=c(0,70)))





#### Quality vs Sample
qualityVSSample <-read_excel("Desktop/Pop1 2.xlsx", sheet = "Quality vs. Sample") #done

#Quality Vs. Sample
dta <- (qualityVSSample
        %>% mutate(CondB=as.factor(CondB),
                   CondA=factor(CondA, levels=c("HighQuality","ModerateQuality","LowQuality")),
                   CondA=recode(CondA,HighQuality="High Quality",ModerateQuality="Moderate Quality", LowQuality="Low Quality"),
                   Population=recode(Population,Pop1="Population A", Pop2="Population B"),
                   Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates"),
                   Criteria=factor(Criteria,levels = c("aBIC","BLRT","BIC","aVLMR","CAIC","ICL","AIC","CLC"))))
# dta_lrt_eb <- (dta 
#                %>% filter(Criteria %in% c("CLC","ICL","BLRT","aVLMR"))
#                %>% mutate(Criteria=factor(Criteria,levels = c("BLRT","aVLMR","ICL","CLC"))))
# dta_IC <- (dta 
#            %>% filter(Criteria %in% c("AIC","BIC","CAIC","aBIC"))
#            %>% mutate(Criteria=factor(Criteria,levels = c("aBIC","BIC","CAIC","AIC"))))
# 
# #LRT/Entropy
# lrt_eb_qualityVSsample <- print(ggplot(dta_lrt_eb)#aes(x=CondB,y=Average,color=Criteria, group=Criteria))
#                                 + geom_point(aes(CondB,Average,color=Criteria,shape=Criteria),size=3) 
#                                 + geom_line(aes(CondB,Average,color=Criteria,group=Criteria,linetype=Criteria),size=1)
#                                 + facet_nested(CondA ~ Population+Covariates,nest_line = TRUE,
#                                                scale="free",
#                                                switch="y",
#                                                labeller = label_wrap_gen(width = 19,multi_line = TRUE))
#                                 + theme_bw()
#                                 + scale_color_brewer(palette="Paired")
#                                 + theme(legend.position = "right",
#                                         legend.justification = "right",
#                                         legend.background = element_rect(color = "grey90", fill = "grey90",size=0.1),
#                                         legend.text = element_text(size=12),
#                                         strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
#                                         strip.text.y = element_text(size = 12),
#                                         legend.title = element_blank(),
#                                         axis.title.x = element_blank(),
#                                         axis.title.y = element_blank(),
#                                         axis.text.x = element_text(size=11, angle=45),
#                                         axis.text.y = element_text(size=11))
#                                 + scale_y_continuous(breaks = seq(0, 100, by = 10))
#                                 + coord_cartesian(ylim=c(0,100)))
# 
# 
# #Information Criteria
# ic_qualityVSsample <- print(ggplot(dta_IC)
#                             + geom_point(aes(x=CondB,y=Average,color=Criteria, shape=Criteria),size=3) 
#                             + geom_line(aes(x=CondB,y=Average,color=Criteria, group=Criteria,linetype=Criteria),size=1)
#                             #+ ggtitle("Average Accuracy Rates of Information Critera")
#                             #+ xlab("Sample Size")
#                             + facet_nested(CondA ~ Population+Covariates,nest_line = TRUE,
#                                            scale="free",
#                                            switch="y",
#                                            labeller = label_wrap_gen(width = 19,multi_line = TRUE))
#                             + theme_bw()
#                             + scale_color_brewer(palette="Set1")
#                             + theme(legend.position = "right",
#                                     legend.justification = "right",
#                                     #legend.position =  c(0.95, 0.23),
#                                     #legend.justification = "right",
#                                     legend.background = element_rect(color = "grey90", fill = "grey90"),
#                                     legend.text = element_text(size=11),
#                                     strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
#                                     strip.text.y = element_text(size = 12),
#                                     legend.title = element_blank(),
#                                     axis.title.x = element_blank(),
#                                     axis.title.y = element_blank(),
#                                     axis.text.x = element_text(size=11,angle=45),
#                                     axis.text.y = element_text(size=11))
#                             #legend.direction = "horizontal")
#                             + scale_y_continuous(breaks = seq(0, 100, by = 10))
#                             + coord_cartesian(ylim=c(0,100)))

# quality_sample <- print(ggplot(dta)
#                             + geom_point(aes(x=CondB,y=Average,color=Criteria, shape=Criteria),size=3)
#                             + geom_line(aes(x=CondB,y=Average,color=Criteria, group=Criteria,linetype=Criteria),size=1)
#                             + facet_nested(CondA ~ Population+Covariates,nest_line = TRUE,
#                                            scale="free",
#                                            switch="y",
#                                            labeller = label_wrap_gen(width = 19,multi_line = TRUE))
#                             + theme_bw()
#                             + scale_shape_manual(values=c(15,20,17,18,19,16,15,3))
#                             + scale_color_manual(values = c("#E41A1C","#FF7F00","#4DAF4A","#984EA3",
#                                                             "#1F78B4","#A6CEE3","#B2DF8A","#33A02C"))
#                             + theme(legend.position = "right",
#                                     legend.justification = "right",
#                                     #legend.position =  c(0.95, 0.23),
#                                     #legend.justification = "right",
#                                     legend.background = element_rect(color = "grey90", fill = "grey90"),
#                                     legend.text = element_text(size=11),
#                                     strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
#                                     strip.text.y = element_text(size = 12),
#                                     legend.title = element_blank(),
#                                     axis.title.x = element_blank(),
#                                     axis.title.y = element_blank(),
#                                     axis.text.x = element_text(size=11,angle=45),
#                                     axis.text.y = element_text(size=11))
#                             #legend.direction = "horizontal")
#                             + scale_y_continuous(breaks = seq(0, 100, by = 10))
#                             + coord_cartesian(ylim=c(0,100)))


quality_sample1 <- print(ggplot(dta)
                        + geom_point(aes(x=Covariates,y=Average,color=Criteria, shape=Criteria),size=3)
                        + geom_line(aes(x=Covariates,y=Average,color=Criteria, group=Criteria),size=1)
                        #+ geom_vline(xintercept = which(dta$Covariates=="No Covariates"))
                        + facet_nested(CondA ~ Population+CondB,nest_line = TRUE,
                                       scale="free",
                                       switch="y",
                                       labeller = label_wrap_gen(width = 19,multi_line = TRUE))
                        + theme_bw()
                        + scale_shape_manual(values=c(15,20,17,18,19,16,15,3))
                        + scale_color_manual(values = c("#E41A1C","#FF7F00","#4DAF4A","#984EA3",
                                                        "#1F78B4","#A6CEE3","#B2DF8A","#33A02C"))
                        + theme(legend.position = "top",
                                #legend.justification = "right",
                                #legend.position =  c(0.95, 0.23),
                                #legend.justification = "right",
                                legend.background = element_rect(color = "grey90", fill = "grey90"),
                                legend.text = element_text(size=11),
                                strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                                strip.text.y = element_text(size = 12),
                                legend.title = element_blank(),
                                panel.grid.minor = element_blank(),
                                #panel.grid.major = element_blank(),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                axis.text.x = element_text(size=9,angle=30,hjust=1),
                                axis.text.y = element_text(size=11))
                        #legend.direction = "horizontal")
                        + scale_y_continuous(breaks = seq(0, 100, by = 15))
                        + coord_cartesian(ylim=c(0,100)))




#############
#
# SPLIT VS QUALITY
#
############

#Split vs Quality 
splitVsQuality <- (read_excel("Desktop/Pop1 2.xlsx", sheet = "Split vs Quality")
                   %>% mutate(Quality=recode(Quality, HighQuality="High Quality", ModerateQuality="Moderate Quality", LowQuality="Low Quality"),
                              Quality=factor(Quality,levels = c("High Quality","Moderate Quality","Low Quality")),
                              Split=recode(Split, Split1="Split 1-(60,35,5)",Split2="Split 2-(45,40,15)"),
                              Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates"),
                              Population=recode(Population,Pop1="Population A", Pop2="Population B"),
                              Criteria=factor(Criteria,levels = c("aBIC","BIC","CAIC","BLRT","aVLMR","AIC","ICL","CLC"))))


split_Quality <- print(ggplot(splitVsQuality)
                          + geom_point(aes(x=Covariates,y=Averages, color=Criteria,shape=Criteria),size=3) 
                          + geom_line(aes(x=Covariates,y=Averages, color=Criteria,group=Criteria),size=1)
                          + facet_nested(Quality~ Population+Split,
                                         nest_line = TRUE,
                                         scale="free",
                                         switch="y",
                                         labeller = label_wrap_gen(width = 20,multi_line = TRUE))
                          + scale_shape_manual(values=c(15,20,17,18,19,16,15,3))
                          + scale_color_manual(values = c("#E41A1C","#FF7F00","#4DAF4A","#984EA3",
                                                       "#1F78B4","#A6CEE3","#B2DF8A","#33A02C"))
                          +theme_bw()
                          + theme_bw()
                          #+ scale_color_brewer(palette="Set1")
                          + theme(legend.position = "right",
                                  legend.justification = "right",
                                  legend.background = element_rect(color = "grey90", fill = "grey90"),
                                  legend.text = element_text(size=11),
                                  strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                                  strip.text.y = element_text(size = 12),
                                  legend.title = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  axis.text.x = element_text(size=11,angle=15,hjust=1),
                                  axis.text.y = element_text(size=11))
                          + scale_y_continuous(breaks = seq(0, 100, by =10 ))
                          + coord_cartesian(ylim=c(0,100)))










##Direct effects 
directeffects<- (read_excel("Desktop/Pop1 2.xlsx", sheet = "DirectEffects")
                   %>% mutate(#Quality=recode(Quality, HighQuality="High Quality", ModerateQuality="Moderate Quality", LowQuality="Low Quality"),
                              #Quality=factor(Quality,levels = c("High Quality","Moderate Quality","Low Quality")),
                              #Split=recode(Split, Split1="Split 1-(60,35,5)",Split2="Split 2-(45,40,15)"),
                              Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates"),
                              Population=recode(Population,Pop1="Population A", Pop2="Population B"),
                              Condition=as.factor(Condition),
                              Criteria=factor(Criteria,levels = c("aBIC","BIC","CAIC","AIC","BLRT","aVLMR","ICL","CLC"))))

deIC <-(directeffects 
        %>% filter(Criteria %in% c("AIC","BIC","aBIC","CAIC"))
        %>% mutate(Criteria=factor(Criteria,levels = c("aBIC","BIC","CAIC","AIC"))))
        
deLRT <- (directeffects %>% filter(Criteria %in% c("BLRT","aVLMR","ICL","CLC"))
          %>% mutate(Criteria=factor(Criteria,levels = c("BLRT","aVLMR","ICL","CLC"))))


de <- print(ggplot()
            + geom_point(data=directeffects,aes(x=Condition,y=Average,color=Criteria,shape=Criteria),size=3)
            + geom_line(data=directeffects,aes(x=Condition,y=Average,color=Criteria,group=Criteria,linetype=Criteria),size=1)
            #+ geom_point(data=deLRT,aes(x=Condition,y=Average,color=Criteria,shape=Criteria),size=3)
            #+ geom_line(data=deLRT,aes(x=Condition,y=Average,group=Criteria,linetype=Criteria),size=1)
            + facet_nested(.~Population + Covariates, 
                           nest_line = TRUE,
                           scale="free",
                           switch="y",
                           labeller = label_wrap_gen(width = 20,multi_line = TRUE))
            + scale_shape_manual(values=c(15,20,17,18,19,16,15,3))
            + scale_color_manual(values = c("#E41A1C","#FF7F00","#4DAF4A","#984EA3",
                                            "#A6CEE3","#1F78B4","#B2DF8A","#33A02C"))
            +theme_bw()
            #+ scale_color_brewer(palette="Set1")
            + theme(legend.position = "right",
                    legend.justification = "right",
                    legend.background = element_rect(color = "grey90", fill = "grey90"),
                    legend.text = element_text(size=11),
                    strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                    strip.text.y = element_text(size = 12),
                    legend.title = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.x = element_text(size=11,angle=45,hjust=1),
                    axis.text.y = element_text(size=11))
            + scale_y_continuous(breaks = seq(0, 100, by =10 ))
            + coord_cartesian(ylim=c(0,70)))


##Sample SIze
samplesize<- (read_excel("Desktop/Pop1 2.xlsx", sheet = "SampleSize")
                 %>% mutate(#Quality=recode(Quality, HighQuality="High Quality", ModerateQuality="Moderate Quality", LowQuality="Low Quality"),
                   #Quality=factor(Quality,levels = c("High Quality","Moderate Quality","Low Quality")),
                   #Split=recode(Split, Split1="Split 1-(60,35,5)",Split2="Split 2-(45,40,15)"),
                   Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates"),
                   Population=recode(Population,Pop1="Population A", Pop2="Population B"),
                   Condition=as.factor(Condition),
                   Criteria=factor(Criteria,levels = c("aBIC","BIC","CAIC","AIC","BLRT","aVLMR","ICL","CLC"))))

deIC <-(directeffects 
        %>% filter(Criteria %in% c("AIC","BIC","aBIC","CAIC"))
        %>% mutate(Criteria=factor(Criteria,levels = c("aBIC","BIC","CAIC","AIC"))))

deLRT <- (directeffects %>% filter(Criteria %in% c("BLRT","aVLMR","ICL","CLC"))
          %>% mutate(Criteria=factor(Criteria,levels = c("BLRT","aVLMR","ICL","CLC"))))


sampleGraph <- print(ggplot()
            + geom_point(data=samplesize,aes(x=Covariates,y=Average,color=Criteria,shape=Criteria),size=3)
            + geom_line(data=samplesize,aes(x=Covariates,y=Average,color=Criteria,group=Criteria,linetype=Criteria),size=1)
            #+ geom_point(data=deLRT,aes(x=Condition,y=Average,color=Criteria,shape=Criteria),size=3)
            #+ geom_line(data=deLRT,aes(x=Condition,y=Average,group=Criteria,linetype=Criteria),size=1)
            + facet_nested(.~Population + Condition, 
                           nest_line = TRUE,
                           scale="free",
                           switch="y",
                           labeller = label_wrap_gen(width = 20,multi_line = TRUE))
            + scale_shape_manual(values=c(15,20,17,18,19,16,15,3))
            + scale_color_manual(values = c("#E41A1C","#FF7F00","#4DAF4A","#984EA3",
                                            "#A6CEE3","#1F78B4","#B2DF8A","#33A02C"))
            +theme_bw()
            #+ scale_color_brewer(palette="Set1")
            + theme(legend.position = "right",
                    legend.justification = "right",
                    legend.background = element_rect(color = "grey90", fill = "grey90"),
                    legend.text = element_text(size=11),
                    strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                    strip.text.y = element_text(size = 12),
                    legend.title = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.x = element_text(size=11,angle=45,hjust=1),
                    axis.text.y = element_text(size=11))
            + scale_y_continuous(breaks = seq(0, 100, by =10 ))
            + coord_cartesian(ylim=c(0,70)))

sample <- c("200","200","200","500","500","500","1000","1000","1000","2000","2000","2000")
dta1 <- (Pop1_2
         %>% separate(col=...1,c("Sample","Quality","a","b","c","d"),sep="&")
         %>% mutate(d=gsub("\\\\","",d),
                    Quality = as.factor(Quality))
         %>% select(-Sample)
         %>% bind_cols(Sample=sample)
         %>% mutate(Sample=as.factor(Sample))
         %>% select(Quality,Sample,a:d))


low <- (dta1 %>% filter(Quality ==" LQ "))
mod <- (dta1 %>% filter(Quality ==" MQ "))
high <-(dta1 %>% filter(Quality ==" HQ "))

new_dta <- bind_rows(low,mod,high)
out <-createWorkbook()
addWorksheet(out,"sheet")
writeData(out,"sheet",new_dta)
saveWorkbook(out,"/Users/sherry/Desktop/new_dta.xlsx")






