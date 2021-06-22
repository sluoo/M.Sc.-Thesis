library(tidyverse)
library(ggh4x)
library(readxl)
library(forcats)

#Load all data 
qualityVSSample <-read_excel("Desktop/Pop1.xlsx", sheet = "Quality vs. Sample") #done

directEffectVsSample <- (read_excel("Desktop/Pop1.xlsx", sheet = "Direct Effects Vs. Sample")
                         %>% mutate(DirectEffects=recode(DirectEffects, `0.4`="Small Effect", `0.9`="Medium Effect", `1.5`="Large Effect"),
                                    Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates"),
                                    Sample=as.factor(Sample),
                                    Population=recode(Population,Pop1="Population A", Pop2="Population B"))) #done

directEffectVsQuality <- (read_excel("Desktop/Pop1.xlsx", sheet = "Direct Effect vs. Quality")
                          %>% mutate(Quality=recode(Quality, HighQuality="High Quality", ModerateQuality="Moderate Quality", LowQuality="Low Quality"),
                                     DirectEffects=recode(DirectEffects, `0.4`="Small Effect", `0.9`="Medium Effect", `1.5`="Large Effect"),
                                     Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates"),
                                     Population=recode(Population,Pop1="Population A", Pop2="Population B"))) #done 

splitVsSample <- (read_excel("Desktop/Pop1.xlsx", sheet = "Split vs Sample")
                  %>% mutate(Splits=recode(Splits, Split1="Split 1-(60,35,5)",Split2="Split 2-(45,40,15)"),
                             Samples=as.factor(Samples),
                             Covariates=recode(Covariats,NoCov="No Covariates", WithCov="With Covariates"),
                             Population=recode(Population,Pop1="Population A", Pop2="Population B")))

splitVsQuality <- (read_excel("Desktop/Pop1.xlsx", sheet = "Split vs Quality")
                   %>% mutate(Quality=recode(Quality, HighQuality="High Quality", ModerateQuality="Moderate Quality", LowQuality="Low Quality"),
                              Quality=factor(Quality,levels = c("High Quality","Moderate Quality","Low Quality")),
                              Split=recode(Split, Split1="Split 1-(60,35,5)",Split2="Split 2-(45,40,15)"),
                              Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates"),
                              population=recode(Population,Pop1="Population A", Pop2="Population B")))

#Quality Vs. Sample
dta <- (qualityVSSample
        %>% mutate(CondB=as.factor(CondB),
                   CondA=factor(CondA, levels=c("HighQuality","ModerateQuality","LowQuality")),
                   CondA=recode(CondA,HighQuality="High Quality",ModerateQuality="Moderate Quality", LowQuality="Low Quality"),
                   Population=recode(Population,Pop1="Population A", Pop2="Population B"),
                   Covariates=recode(Covariates,NoCov="No Covariates", WithCov="With Covariates")))
dta_lrt_eb <- (dta 
               %>% filter(Criteria %in% c("CLC","ICL","BLRT","aVLMR"))
               %>% mutate(Criteria=factor(Criteria,levels = c("BLRT","aVLMR","ICL","CLC"))))
dta_IC <- (dta 
           %>% filter(Criteria %in% c("AIC","BIC","CAIC","aBIC"))
           %>% mutate(Criteria=factor(Criteria,levels = c("BIC","aBIC","CAIC","AIC"))))

#LRT/Entropy
lrt_eb_qualityVSsample <- print(ggplot(dta_lrt_eb)#aes(x=CondB,y=Average,color=Criteria, group=Criteria))
           + geom_point(aes(CondB,Average,color=Criteria,shape=Criteria),size=3) 
           + geom_line(aes(CondB,Average,color=Criteria,group=Criteria,linetype=Criteria),size=1)
           + facet_nested(CondA ~ Population+Covariates,nest_line = TRUE,
                          scale="free",
                          switch="y")
           + theme_bw()
           + scale_color_brewer(palette="Paired")
           + theme(legend.position = "top",
                   legend.justification = "left",
                   legend.text = element_text(size=11),
                   strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                   strip.text.y = element_text(size = 12),
                   legend.title = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_text(size=11, angle=45),
                   axis.text.y = element_text(size=11))
           + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
           + coord_cartesian(ylim=c(0,100)))


#Information Criteria
ic_qualityVSsample <- print(ggplot(dta_IC)
                                + geom_point(aes(x=CondB,y=Average,color=Criteria, shape=Criteria),size=3) 
                                + geom_line(aes(x=CondB,y=Average,color=Criteria, group=Criteria,linetype=Criteria),size=1)
                                #+ ggtitle("Average Accuracy Rates of Information Critera")
                                #+ xlab("Sample Size")
                                + facet_nested(CondA ~ Population+Covariates,nest_line = TRUE,
                                               scale="free",
                                               switch="y")
                                + theme_bw()
                                + scale_color_brewer(palette="Set1")
                                + theme(legend.position = "top",
                                        legend.justification = "left",
                                        legend.text = element_text(size=11),
                                        strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                                        strip.text.y = element_text(size = 12),
                                        legend.title = element_blank(),
                                        axis.title.x = element_blank(),
                                        axis.title.y = element_blank(),
                                        axis.text.x = element_text(size=11,angle=45),
                                        axis.text.y = element_text(size=11))
                            + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                            + coord_cartesian(ylim=c(0,100)))



scale_y_continuous(breaks = seq(0, 1.2, 0.2)) + 
  coord_cartesian(ylim = c(0, 1.2))

#Direct Effect Vs. Quality 
directEffectVsQuality_LRT_EB <- (directEffectVsQuality
                                 %>% filter(Criteria %in% c("CLC","ICL","BLRT","aVLMR"))
                                 %>% mutate(Quality=factor(Quality, levels=c("High Quality","Moderate Quality","Low Quality")),
                                            DirectEffects=factor(DirectEffects, levels=c("Small Effect","Medium Effect","Large Effect"))))

directEffectVsQuality_IC <- (directEffectVsQuality 
                                 %>% filter(Criteria %in% c("BIC","aBIC","CAIC","AIC"))
                                 %>% mutate(Quality=factor(Quality, levels=c("High Quality","Moderate Quality","Low Quality")),
                                            DirectEffects=factor(DirectEffects, levels=c("Small Effect","Medium Effect","Large Effect"))))
#Graph
dEvQ_LRT_EB <- print(ggplot(directEffectVsQuality_LRT_EB)
                     + geom_point(aes(x=DirectEffects,y=Average, color=Criteria,shape=Criteria),size=3) 
                     + geom_line(aes(x=DirectEffects,y=Average, color=Criteria,group=Criteria,linetype=Criteria),size=1)
                     + facet_nested(Quality ~ Population+Covariates,
                                    nest_line = TRUE,
                                    scale="free",
                                    switch="y")
                     + theme_bw()
                     + scale_color_brewer(palette="Paired")
                     + theme(legend.position = "top",
                             legend.justification = "left",
                             legend.text = element_text(size=11),
                             strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                             strip.text.y = element_text(size = 12),
                             legend.title = element_blank(),
                             axis.title.x = element_blank(),
                             axis.title.y = element_blank(),
                             axis.text.x = element_text(size=11, angle=13,hjust=0.7),
                             axis.text.y = element_text(size=11))
                     + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                     + coord_cartesian(ylim=c(0,100)))

dEvQ_IC <- print(ggplot(directEffectVsQuality_IC)
                 + geom_point(aes(x=DirectEffects,y=Average, color=Criteria,shape=Criteria),size=3) 
                 + geom_line(aes(x=DirectEffects,y=Average, color=Criteria,group=Criteria, linetype=Criteria),size=1)
                 + facet_nested(Quality ~ Population+Covariates,
                                nest_line = TRUE,
                                scale="free",
                                switch="y")
                 + theme_bw()
                 + scale_color_brewer(palette="Set1")
                 + theme(legend.position = "top",
                         legend.justification = "left",
                         legend.text = element_text(size=11),
                         strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                         strip.text.y = element_text(size = 12),
                         legend.title = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         axis.text.x = element_text(size=11,angle=13,hjust=0.7),
                         axis.text.y = element_text(size=11))
                 + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                 + coord_cartesian(ylim=c(0,100)))

#Direct Effects Vs.Sample 
directEffectVsSample_LRT_EB <- (directEffectVsSample
                                 %>% filter(Criteria %in% c("CLC","ICL","BLRT","aVLMR"))
                                 %>% mutate(DirectEffects=factor(DirectEffects, levels=c("Small Effect","Medium Effect","Large Effect"))))

directEffectVsSample_IC <- (directEffectVsSample
                                %>% filter(Criteria %in% c("BIC","aBIC","CAIC","AIC"))
                                %>% mutate(DirectEffects=factor(DirectEffects, levels=c("Small Effect","Medium Effect","Large Effect"))))

#Graph
dEvS_LRT_EB <- print(ggplot(directEffectVsSample_LRT_EB)
                     + geom_point(aes(x=Sample,y=Average, color=Criteria,shape=Criteria),size=3) 
                     + geom_line(aes(x=Sample,y=Average, color=Criteria,group=Criteria,linetype=Criteria),size=1)
                     + facet_nested(DirectEffects ~ Population+Covariates,
                                    nest_line = TRUE,
                                    scale="free",
                                    switch="y")
                     + theme_bw()
                     + scale_color_brewer(palette="Paired")
                     + theme(legend.position = "top",
                             legend.justification = "left",
                             legend.text = element_text(size=11),
                             strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                             strip.text.y = element_text(size = 12),
                             legend.title = element_blank(),
                             axis.title.x = element_blank(),
                             axis.title.y = element_blank(),
                             axis.text.x = element_text(size=11),
                             axis.text.y = element_text(size=11))
                     + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                     + coord_cartesian(ylim=c(0,100)))
                     

dEvS_IC <- print(ggplot(directEffectVsSample_IC)
                 + geom_point(aes(x=Sample,y=Average, color=Criteria,shape=Criteria),size=3) 
                 + geom_line(aes(x=Sample,y=Average, color=Criteria,group=Criteria, linetype=Criteria),size=1)
                 + facet_nested( DirectEffects~ Population+Covariates,
                                nest_line = TRUE,
                                scale="free",
                                switch="y")
                 + theme_bw()
                 + scale_color_brewer(palette="Set1")
                 + theme(legend.position = "top",
                         legend.justification = "left",
                         legend.text = element_text(size=11),
                         strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                         strip.text.y = element_text(size = 12),
                         legend.title = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         axis.text.x = element_text(size=11),
                         axis.text.y = element_text(size=11))
                 + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                 + coord_cartesian(ylim=c(0,100)))

#Split vs Sample
splitVsSample_LRT_EB <- (splitVsSample
                         %>% filter(Criteria %in% c("CLC","ICL","BLRT","aVLMR")))

splitVsSample_IC <- (splitVsSample
                         %>% filter(Criteria %in% c("BIC","aBIC","CAIC","AIC")))

split_Sample_LRT_EB <- print(ggplot(splitVsSample_LRT_EB)
                    + geom_point(aes(x=Samples,y=Averages, color=Criteria,shape=Criteria),size=3) 
                    + geom_line(aes(x=Samples,y=Averages, color=Criteria,group=Criteria, linetype=Criteria),size=1)
                    + facet_nested( Splits~ Population+Covariates,
                                    nest_line = TRUE,
                                    scale="free",
                                    switch="y")
                    + theme_bw()
                    + scale_color_brewer(palette="Set1")
                    + theme(legend.position = "top",
                            legend.justification = "left",
                            legend.text = element_text(size=11),
                            strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                            strip.text.y = element_text(size = 12),
                            legend.title = element_blank(),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                                     axis.text.x = element_text(size=11),
                                     axis.text.y = element_text(size=11))
                    + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                    + coord_cartesian(ylim=c(0,100)))

split_Sample_IC <- print(ggplot(splitVsSample_IC)
                             + geom_point(aes(x=Samples,y=Averages, color=Criteria,shape=Criteria),size=3) 
                             + geom_line(aes(x=Samples,y=Averages, color=Criteria,group=Criteria, linetype=Criteria),size=1)
                             + facet_nested( Splits~ Population+Covariates,
                                             nest_line = TRUE,
                                             scale="free",
                                             switch="y")
                             + theme_bw()
                             + scale_color_brewer(palette="Set1")
                             + theme(legend.position = "top",
                                     legend.justification = "left",
                                     legend.text = element_text(size=11),
                                     strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                                     strip.text.y = element_text(size = 12),
                                     legend.title = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.text.x = element_text(size=11),
                                     axis.text.y = element_text(size=11))
                         + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                         + coord_cartesian(ylim=c(0,100)))

#Split vs Quality 
splitVsQuality_LRT_EB <- (splitVsQuality
                         %>% filter(Criteria %in% c("CLC","ICL","BLRT","aVLMR")))

splitVsQuality_IC <- (splitVsQuality
                     %>% filter(Criteria %in% c("BIC","aBIC","CAIC","AIC")))

split_Quality_LRT_EB <- print(ggplot(splitVsQuality_LRT_EB)
                             + geom_point(aes(x=Quality,y=Averages, color=Criteria,shape=Criteria),size=3) 
                             + geom_line(aes(x=Quality,y=Averages, color=Criteria,group=Criteria, linetype=Criteria),size=1)
                             + facet_nested( Split~ Population+Covariates,
                                             nest_line = TRUE,
                                             scale="free",
                                             switch="y")
                             + theme_bw()
                             + scale_color_brewer(palette="Paired")
                             + theme(legend.position = "top",
                                     legend.justification = "left",
                                     legend.text = element_text(size=11),
                                     strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                                     strip.text.y = element_text(size = 12),
                                     legend.title = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.text.x = element_text(size=11,angle=45,hjust=1),
                                     axis.text.y = element_text(size=11))
                             + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                             + coord_cartesian(ylim=c(0,100)))

split_Quality_IC <- print(ggplot(splitVsQuality_IC)
                              + geom_point(aes(x=Quality,y=Averages, color=Criteria,shape=Criteria),size=3) 
                              + geom_line(aes(x=Quality,y=Averages, color=Criteria,group=Criteria, linetype=Criteria),size=1)
                              + facet_nested( Split~ Population+Covariates,
                                              nest_line = TRUE,
                                              scale="free",
                                              switch="y")
                              + theme_bw()
                              + scale_color_brewer(palette="Set1")
                              + theme(legend.position = "top",
                                      legend.justification = "left",
                                      legend.text = element_text(size=11),
                                      strip.text.x = element_text(size = 12), #colour = "orange", #angle = 90),
                                      strip.text.y = element_text(size = 12),
                                      legend.title = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      axis.text.x = element_text(size=11,angle=45,hjust=1),
                                      axis.text.y = element_text(size=11))
                          + scale_y_continuous(breaks = seq(0, 100, by = 10.5))
                          + coord_cartesian(ylim=c(0,100)))










