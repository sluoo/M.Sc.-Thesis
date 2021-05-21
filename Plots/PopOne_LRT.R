#Constants: HQ,1.5,Split2

library(tidyverse)
dta <- (pop1lrt 
        %>% pivot_longer(-c(`Sample Size`), 
        names_to = "lrt", values_to = "values")
        %>% separate(lrt,into=c("cov","lrt"))
        %>% mutate(cov = as.factor(cov),
                   lrt = as.factor(lrt),
                  sample = as.factor(`Sample Size`)))

dta$lrt <- recode_factor(dta$lrt, avlmr="aVLMR", blrt="BLRT")

t <- print(dta %>% ggplot(aes(x=lrt,y=values))
      + geom_bar(aes(alpha=cov,fill=lrt),position="dodge",
                 stat="identity",width =0.8)
      + facet_grid(~sample)
      #+ labs(x="Information Criterion",y="Percentage",title = "Percentage of Correct")
      + scale_alpha_manual(values = c(0.5, 1),
                           labels = c("Without Covariates","With Covariates"))
      + theme_bw()
      + scale_fill_manual(values = c("#2e4057","#00798c","#60B8D9"))
      + theme(legend.position = "bottom", 
              legend.text = element_text(size=10),
              #panel.spacing = unit(1, "lines"),
              legend.title = element_blank(),
              legend.key.size = unit(7,"mm"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              #axis.title.y = element_text(size=10),
              axis.text.x = element_text(angle=360))
      + guides(fill=FALSE))
