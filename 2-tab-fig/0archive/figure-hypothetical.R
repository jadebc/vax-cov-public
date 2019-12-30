##############################################
# Shoo the Flu Evaluation
# 2014-2016

# figure showing what we expect to find
##############################################
rm(list=ls())
library(ggplot2)
library(reshape2)
source("~/Documents/CRG/flu/absentee/4-figures/theme_complete_bw.R")

df=data.frame(y=c(0,0,-0.2),x=c("2014-15","2015-16","2016-17"))
df$lb=c(-0.05,-0.05,-0.25)
df$ub=c(0.05,0.05,-0.15)

pdf(file="~/Dropbox/Flu/StFData/2016-2017/Results/hypothetical-results.pdf",
    width=6,height=4)
ggplot(df,aes(x=x,y=y))+geom_point()+geom_errorbar(aes(ymin=lb,ymax=ub),width=0.1)+
  theme_complete_bw()+xlab("")+ylab("Difference (95% CI)")+
  scale_y_continuous(limits=c(-.3,.3))+geom_hline(yintercept=0,linetype="dashed")
dev.off()