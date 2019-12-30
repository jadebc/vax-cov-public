############################################
# Vaccine coverage survey conducted in March 2017
# Table of vaccine coverage, RD, race, edu
############################################

rm(list=ls())
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_results.RData")
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_std_results.RData")
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/0-base-functions-tabfig.R")

tab.dir="~/Dropbox/Flu/StFData/Vax cov/Tables/"

# Format results for % vaccinated overall
vx.ousd=rbind(vx.y1.o,vx.y2.o,vx.y3.o)
vx.ousd=vx.ousd[,c("Mean","Lower 95%CI","Upper 95%CI")]
vx.ousd=apply(vx.ousd,1,pt.est.ci.f,0,100)
vx.ousd.N=vx.y1.o$N
  
vx.wccusd=rbind(vx.y1.w,vx.y2.w,vx.y3.w)
vx.wccusd=vx.wccusd[,c("Mean","Lower 95%CI","Upper 95%CI")]
vx.wccusd=apply(vx.wccusd,1,pt.est.ci.f,0,100)
vx.wccusd.N=vx.y1.w$N

# Format results for % vaccinated by race
vx.y1.o.race.f=apply(vx.y1.o.race[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)
vx.y2.o.race.f=apply(vx.y2.o.race[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)
vx.y3.o.race.f=apply(vx.y3.o.race[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)

vx.y1.w.race.f=apply(vx.y1.w.race[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)
vx.y2.w.race.f=apply(vx.y2.w.race[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)
vx.y3.w.race.f=apply(vx.y3.w.race[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)

# Format results for % vaccinated by edu
vx.y1.o.edu.f=apply(vx.y1.o.edu[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)
vx.y2.o.edu.f=apply(vx.y2.o.edu[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)
vx.y3.o.edu.f=apply(vx.y3.o.edu[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)

vx.y1.w.edu.f=apply(vx.y1.w.edu[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)
vx.y2.w.edu.f=apply(vx.y2.w.edu[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)
vx.y3.w.edu.f=apply(vx.y3.w.edu[,c("Mean","LowerCI","UpperCI")],1,pt.est.ci.f,0,100)

# construct table 
all=c(vx.ousd.N,vx.ousd,vx.wccusd.N,vx.wccusd)
race=cbind(vx.y1.o.race$N,vx.y1.o.race.f, vx.y2.o.race.f,vx.y3.o.race.f,
           vx.y1.w.race$N,vx.y1.w.race.f, vx.y2.w.race.f,vx.y3.w.race.f)

edu=cbind(vx.y1.o.edu$N,vx.y1.o.edu.f, vx.y2.o.edu.f,vx.y3.o.edu.f,
          vx.y1.w.edu$N,vx.y1.w.edu.f, vx.y2.w.edu.f,vx.y3.w.edu.f)

vx.tab=as.data.frame(rbind(all,race,edu))
colnames(vx.tab)=c("ousd-N","ousd-y1","ousd-y2","ousd-y3",
                   "wccusd-N","wccusd-y1","wccusd-y2","wccusd-y3")
vx.tab$label=c("All students","~~~White","~~~African American",
               "~~~Latino","~~~Asian","~~~Native American","~~~Pacific islander",
               "~~~Multiple races","~~~Not reported",
               "~~~Less than high school","~~~High school",
               "~~~Associate/College","~~~Postgraduate","~~~Not reported")

vx.tab=vx.tab[,c(9,1:8)]
vx.tab=vx.tab[c(1:5,7,6,8:14),]

save(vx.tab,file=paste0(tab.dir,"tab-vax.RData"))
