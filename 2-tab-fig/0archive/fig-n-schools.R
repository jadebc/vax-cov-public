# bar chart of number of SLIV schools in each study

d=read.csv("~/Dropbox/Flu/StFData/Vax cov/SLIV_studies.csv")
library(ggplot2)
library(dplyr)

d = d %>% filter(!is.na(Num_SLIV_schools)) %>%
  mutate(Study=as.factor(Study)) %>%
  mutate(rigorous = as.factor(ifelse(controlled_study=="yes" & 
                             (matched=="yes" | randomized=="yes"),1,0)))

ggplot(d, aes(x=Study, y=Num_SLIV_schools))+
  geom_col(aes(fill=rigorous), col="black")+coord_flip()+
  xlab("Number of SLIV schools")+
  scale_fill_manual(values=c("gray","black"))+
  theme_bw()