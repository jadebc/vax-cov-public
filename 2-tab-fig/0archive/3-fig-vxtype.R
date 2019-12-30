########################################
# Vaccine coverage survey conducted in March 2017
# Create figures for presentation
# vaccination type by year
########################################

rm(list=ls())
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_results.RData")
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_results_1718.RData")

source("~/Documents/CRG/flu/vax-cov/3-tab-fig/theme_complete_bw.R")
library(ggplot2)

plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"

shot.comp.y1.o$type="Shot"
spray.comp.y1.o$type="Spray"
error.comp.y1.o$type="Error/missing/don't know"

shot.comp.y1.o$dist="OUSD"
spray.comp.y1.o$dist="OUSD"
error.comp.y1.o$dist="OUSD"

shot.comp.y1.o$yr="2014-15"
spray.comp.y1.o$yr="2014-15"
error.comp.y1.o$yr="2014-15"

shot.comp.y1.w$type="Shot"
spray.comp.y1.w$type="Spray"
error.comp.y1.w$type="Error/missing/don't know"

shot.comp.y1.w$dist="WCCUSD"
spray.comp.y1.w$dist="WCCUSD"
error.comp.y1.w$dist="WCCUSD"

shot.comp.y1.w$yr="2014-15"
spray.comp.y1.w$yr="2014-15"
error.comp.y1.w$yr="2014-15"

shot.comp.y2.o$type="Shot"
spray.comp.y2.o$type="Spray"
error.comp.y2.o$type="Error/missing/don't know"

shot.comp.y2.o$dist="OUSD"
spray.comp.y2.o$dist="OUSD"
error.comp.y2.o$dist="OUSD"

shot.comp.y2.o$yr="2015-16"
spray.comp.y2.o$yr="2015-16"
error.comp.y2.o$yr="2015-16"

shot.comp.y2.w$type="Shot"
spray.comp.y2.w$type="Spray"
error.comp.y2.w$type="Error/missing/don't know"

shot.comp.y2.w$dist="WCCUSD"
spray.comp.y2.w$dist="WCCUSD"
error.comp.y2.w$dist="WCCUSD"

shot.comp.y2.w$yr="2015-16"
spray.comp.y2.w$yr="2015-16"
error.comp.y2.w$yr="2015-16"

shot.comp.y3.o$type="Shot"
spray.comp.y3.o$type="Spray"
error.comp.y3.o$type="Error/missing/don't know"

shot.comp.y3.o$dist="OUSD"
spray.comp.y3.o$dist="OUSD"
error.comp.y3.o$dist="OUSD"

shot.comp.y3.o$yr="2016-17"
spray.comp.y3.o$yr="2016-17"
error.comp.y3.o$yr="2016-17"

shot.comp.y3.w$type="Shot"
spray.comp.y3.w$type="Spray"
error.comp.y3.w$type="Error/missing/don't know"

shot.comp.y3.w$dist="WCCUSD"
spray.comp.y3.w$dist="WCCUSD"
error.comp.y3.w$dist="WCCUSD"

shot.comp.y3.w$yr="2016-17"
spray.comp.y3.w$yr="2016-17"
error.comp.y3.w$yr="2016-17"

shot.comp.y4.o$type="Shot"
error.comp.y4.o$type="Error/missing/don't know"

shot.comp.y4.o$dist="OUSD"
error.comp.y4.o$dist="OUSD"

shot.comp.y4.o$yr="2017-18"
error.comp.y4.o$yr="2017-18"

shot.comp.y4.w$type="Shot"
error.comp.y4.w$type="Error/missing/don't know"

shot.comp.y4.w$dist="WCCUSD"
error.comp.y4.w$dist="WCCUSD"

shot.comp.y4.w$yr="2017-18"
error.comp.y4.w$yr="2017-18"

type.graph=rbind(shot.comp.y1.o,spray.comp.y1.o,error.comp.y1.o,shot.comp.y1.w,spray.comp.y1.w,error.comp.y1.w,
                 shot.comp.y2.o,spray.comp.y2.o,error.comp.y2.o,shot.comp.y2.w,spray.comp.y2.w,error.comp.y2.w,
                 shot.comp.y3.o,spray.comp.y3.o,error.comp.y3.o,shot.comp.y3.w,spray.comp.y3.w,error.comp.y3.w,
                 shot.comp.y4.o,error.comp.y4.o,shot.comp.y4.w,error.comp.y4.w)

type.graph$yr=as.factor(type.graph$yr)
type.graph$Mean=type.graph$Mean*100
type.graph$Mean.f=sprintf("%0.0f",type.graph$Mean)

colnames(type.graph)[5:6]=c("lower","upper")
type.graph$lower=type.graph$lower*100
type.graph$upper=type.graph$upper*100

type.graph$type=factor(type.graph$type,levels=c("Shot","Spray",
      "Error/missing/don't know"))
levels(type.graph$type)[3]="Error/\nMissing/\nDon't know"
type.graph$Mean.f[type.graph$yr=="2016-17" & type.graph$dist=="WCCUSD" & type.graph$type=="Spray"]=""
type.graph$Mean.f[type.graph$type=="Error/\nMissing/\nDon't know"]=""
type.graph$printmean=c(42,15,0,
                       42,9,0,
                       
                       42,17,0,
                       42,9,0,
                       
                       42,8,0,
                       42,43,0,
                       
                       42,0,
                       42,0)

pdf(file=paste0(plot.dir,"fig-vxtype-y4.pdf"),width=7.5,height=4)
ggplot(type.graph,aes(y=Mean,x=dist,fill=type))+geom_bar(stat="identity")+
  scale_fill_manual("",values=c("#2185c5","#ff9715","#677480"))+
  theme_complete_bw()+ylab("Percent")+xlab("")+
  facet_grid(~yr)+
  scale_y_continuous(breaks=seq(0,70,5),labels=seq(0,70,5))+
  geom_text(mapping=aes(label=Mean.f,y=printmean),size=3,show.legend=FALSE)
dev.off()


