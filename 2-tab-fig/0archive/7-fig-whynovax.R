########################################
# Vaccine coverage survey conducted in Feb 2018
# figure: reasons for not vaccinating
########################################

rm(list=ls())
library(dplyr)
library(ggplot2)
library(reshape2)
load("~/Dropbox/Flu/StFData/2017-2018/Data/Temp/vxcov-import.RData")
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_results_1718.RData")

plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"

# Get percentages for slide on reasons not vax
# and potential vax level

# beliefs
belief = data.import %>% 
  filter(district=="OUSD") %>%
  mutate(belief=ifelse(whynot_1==1| whynot_8==1,1,0)) %>%
  summarize(mean(belief))
  
# trust
trust = data.import %>% 
  filter(district=="OUSD") %>%
  mutate(trust=ifelse(whynot_3==1| whynot_8==5 | whynot_6==1,1,0)) %>%
  summarize(mean(trust))

# cost
cost = data.import %>% 
  filter(district=="OUSD") %>%
  mutate(cost=ifelse(whynot_2==1| whynot_4==1|
                       whynot_7==1|whynot_9==1|
                       whynot_10==1|whynot_11==1|
                       whynot_12==1,1,0)) %>%
  summarize(mean(cost,na.rm=TRUE))

# -------------------------------
# Plot with entire list of reasons
# -------------------------------
graph.data = data.import %>%
  filter(vx1718==0) %>%
  select(district,whynot_1:whynot_12) %>%
  group_by(district) %>%
  summarise_all(mean, na.rm=TRUE) 

graph.data[,c(2:13)]=graph.data[,c(2:13)]*100

graph.data.l = melt(graph.data) %>%
  mutate(lab=case_when(
    variable=="whynot_1" ~ "I don't believe in it",
    variable=="whynot_2" ~ "It costs too much",
    variable=="whynot_3" ~ "My student is afraid of needles",
    variable=="whynot_4" ~ "I didn't know where to get it",
    variable=="whynot_5" ~ "I didn't trust schools to vaccinate my student",
    variable=="whynot_6" ~ "Our doctor did not recommend it",
    variable=="whynot_7" ~ "I didn't have time to take my student to the doctor",
    variable=="whynot_8" ~ "I believe it might make my student sick",
    variable=="whynot_9" ~ "I thought my student needed health insurance to get it",
    variable=="whynot_10" ~ "I didn't receive the consent form to get thevaccine at school",
    variable=="whynot_11" ~ "I forgot to return the consent form to get the vaccine at school",
    variable=="whynot_12" ~ "I didn't want to share my insurance\ninformation on the consent form to get the vaccine at school"
  ))

# sort by most common reasons
ord.lev=graph.data.l$lab[graph.data.l$dist=="OUSD"][order(
  graph.data.l$val[graph.data.l$dist=="OUSD"])]

graph.data.l <- graph.data.l %>%
  mutate(lab.f = factor(graph.data.l$lab, levels = ord.lev)) %>%
  mutate(value.f = paste0(sprintf("%0.00f",value),"%"))

pdf(file=paste0(plot.dir,"fig-novx-why-bar.pdf"),width=10,height=5)
ggplot(graph.data.l, aes(x=lab.f,y=value,fill=district))+
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip()+
  scale_y_continuous(limits=c(0,40))+ylab("Percentage")+
  xlab("Reason child did not receive influenza vaccine in 2017-18")+
  scale_fill_manual("",values=c("#2185c5","#ff9715"))+
  geom_text(aes(label=value.f), hjust=-0.05,
            position=position_dodge(width=1))
dev.off()

# -------------------------------
# Plot with two bins
# -------------------------------
whynot.cost.res.o$dist="OUSD"
whynot.cost.res.w$dist="WCCUSD"

whynot.cost.res.o$type="Cost-related reasons"
whynot.cost.res.w$type="Cost-related reasons"

whynot.trust.res.o$dist="OUSD"
whynot.trust.res.w$dist="WCCUSD"

whynot.trust.res.o$type="Trust-related reasons"
whynot.trust.res.w$type="Trust-related reasons"

two.graph=as.data.frame(rbind(whynot.cost.res.o,whynot.cost.res.w,
                              whynot.trust.res.o,whynot.trust.res.w))
colnames(two.graph)[5:6]=c("lb","ub")
two.graph <- two.graph %>%
  mutate(Mean=Mean*100,
         lb=lb*100,
         ub=ub*100) %>%
  mutate(type=ifelse(type=="Cost-related reasons", 
                     "Cost/logistics\nreasons",type)) %>%
  mutate(type = ifelse(type=="Trust-related reasons",
                       "Trust/belief\nreasons",type))
two.graph$type = as.factor(two.graph$type)


pdf(file=paste0(plot.dir,"fig-novx-why-cat.pdf"),width=5,height=3)
ggplot(two.graph, aes(x=type,y=Mean))+
  geom_point(aes(col=dist),size=2,position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=lb, ymax=ub, col=dist),
                position=position_dodge(width=0.5),
                width=0.15)+
  theme_bw()+ylab("Percentage (95% CI)")+
  xlab("Category of reason child was not\nvaccinated for influenza in 2017-18")+
  scale_y_continuous(limits=c(0,70))+
  scale_color_manual("District",values=c("#2185c5","#ff9715"))
dev.off()


