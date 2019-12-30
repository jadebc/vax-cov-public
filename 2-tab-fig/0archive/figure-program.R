##############################################
# Shoo the Flu Evaluation
# 2014-2016

# figure showing program activities
##############################################
rm(list=ls())
library(ggplot2)
library(reshape2)
library(dplyr)
source("~/Documents/CRG/flu/absentee/4-figures/theme_complete_bw.R")
plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"

df=data.frame(studentLAIV=c(6653,8547,0,0),yr=c("2014-15","2015-16","2016-17","2017-18"))
df$studentIIV=c(932,1559,7502,7536)
df$adultLAIV=c(464,101,0,0)
df$adultIIV=c(593,1070,1230,1279)

df.l=melt(df,id.vars="yr")
df.l$student="Student"
df.l$student[df.l$variable=="adultLAIV"|df.l$variable=="adultIIV"]="Adult"
df.l$type="LAIV"
df.l$type[df.l$variable=="studentIIV"|df.l$variable=="adultIIV"]="IIV"

pdf(file=paste0(plot.dir,"stf.activity.pdf"),
    width=8,height=3)
ggplot(df.l,aes(x=yr,y=value,group=type))+
  geom_bar(aes(fill=type),stat="identity",width=0.6)+
  ylab("Number of vaccinations delivered")+
  xlab("")+theme_complete_bw()+
  facet_grid(~student)+
  scale_fill_manual("",values=c("#2185c5","#ff9715"))+
  scale_y_continuous(limits=c(0,12000))
dev.off()