dta <- citem 
library(tidyverse)
library(cowplot)

c1 <- (dta %>% gather(key=C1, value=c1_prob, HQ1,MQ1,LQ1) %>% select(Items,C1,c1_prob))
c2 <- (dta %>% gather(key=C2, value=c2_prob, HQ2,MQ2,LQ2) %>% select(Items,C2,c2_prob))
c3 <- (dta %>% gather(key=C3, value=c3_prob, HQ3,MQ3,LQ3) %>% select(Items,C3,c3_prob))


dta2 <- as.tibble(data.frame(c1,c2,c3)  %>% gather(key = Class, value = Quality, C1,C2,C3)
         %>% gather(key = class_prob, value = prob_values, c1_prob, c2_prob, c3_prob)
         %>% mutate(Quality = ifelse(Quality %in% c("HQ1","HQ2","HQ3"),"HQ",Quality))
         %>% mutate(Quality = ifelse(Quality %in% c("LQ1","LQ2","LQ3"),"LQ",Quality))
         %>% mutate(Quality = ifelse(Quality %in% c("MQ1","MQ2","MQ3"),"MQ",Quality))
         %>% mutate(Class = as.factor(Class))
         %>% mutate(class_prob =as.factor(class_prob))
         %>% mutate(Quality = as.factor(Quality)))


dta2$Quality1 = factor(dta2$Quality, levels=c('HQ','MQ','LQ'),
                       labels = c("High Quality","Moderate Quality","Low Quality"))

d <- print(ggplot(dta2,aes(x=factor(Items),y=prob_values,group=class_prob))
           + geom_point()
           + geom_line(aes(linetype=class_prob))
           + scale_linetype_manual(values=c("solid","dashed","dotted"),
                                   labels=c("C1","C2","C3"))
           + labs(x="Latent Class Items",y="Conditional Item Probability",title = "Item-Response Plots")
           + labs(shape="Latent Class",linetype="Latent Classes")
           + theme_bw(10)
           + theme(legend.position = "bottom", 
                   legend.text = element_text(size=8),
                   #panel.spacing = unit(0.5, "lines"),
                   legend.title = element_text(size=9))
           + facet_grid(.~Quality1))





