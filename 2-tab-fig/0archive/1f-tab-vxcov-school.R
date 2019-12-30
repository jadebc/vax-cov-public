############################################
# Vaccine coverage survey conducted in March 2017
# RD with and without standardization
############################################

rm(list=ls())
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_school_results.RData")
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/0-base-functions-tabfig.R")

tab.dir="~/Dropbox/Flu/StFData/Vax cov/Tables/"

y1.tab=merge(vx.y1.school,nvax.y1,by="schoolname")
y1.tab$res=apply(y1.tab[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,decimals=0,scale=100)
y1.tab=y1.tab[,c("schoolname","dist","n","N","res")]

y2.tab=merge(vx.y2.school,nvax.y2,by="schoolname")
y2.tab$res=apply(y2.tab[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,decimals=0,scale=100)
y2.tab=y2.tab[,c("schoolname","dist","n","N","res")]

y3.tab=merge(vx.y3.school,nvax.y3,by="schoolname")
y3.tab$res=apply(y3.tab[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,decimals=0,scale=100)
y3.tab=y3.tab[,c("schoolname","dist","n","N","res")]

tab.sch=merge(y1.tab,y2.tab,by=c("schoolname","dist"))
tab.sch=merge(tab.sch,y3.tab,by=c("schoolname","dist"))
colnames(tab.sch)=c("schoolname","dist","n.y1","N.y1",
    "res.y1","n.y2","N.y2","res.y2","n.y3","N.y3","res.y3")
tab.sch=tab.sch[order(tab.sch$dist,tab.sch$res.y1),]

save(tab.sch,file=paste0(tab.dir,"tab-vax-school.RData"))
