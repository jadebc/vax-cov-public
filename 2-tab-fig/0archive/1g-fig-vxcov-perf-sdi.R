########################################
# Vaccine coverage survey conducted in March 2017
# Create figures for presentation
# vaccination coverage by year

# stratify by school stf performance and distress
########################################

rm(list=ls())
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_perf_sdi.RData")
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/theme_complete_bw.R")
library(ggplot2)

plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"

#---------------------------------------
# School distress
#---------------------------------------
vx.y1.o.sdi$yr="2014-15"
vx.y2.o.sdi$yr="2015-16"
vx.y3.o.sdi$yr="2016-17"

vx.graph.sdi=rbind(vx.y1.o.sdi,vx.y2.o.sdi,vx.y3.o.sdi)

vx.graph.sdi$yr=as.factor(vx.graph.sdi$yr)
vx.graph.sdi$Mean=vx.graph.sdi$Mean*100
vx.graph.sdi$mean.f=sprintf("%0.0f",vx.graph.sdi$Mean)
vx.graph.sdi$type=rep(c("Low distress","High distress"),3)
vx.graph.sdi$type=factor(vx.graph.sdi$type)

vx.graph.sdi$lb=vx.graph.sdi$LowerCI*100
vx.graph.sdi$ub=vx.graph.sdi$UpperCI*100

pdf(file=paste0(plot.dir,"fig-vxcov-sdi.pdf"),width=8,height=3)
ggplot(vx.graph.sdi,aes(x=yr,y=Mean,group=type))+
  geom_point(aes(shape=type,col=type),
             size=3,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=lb,ymax=ub,col=type),
                position=position_dodge(width=0.4),size=0.8, width=0.15)+
  ylab("Percent vaccinated for influenza")+xlab("")+
  scale_y_continuous(limits=c(50,80),labels=seq(50,80,5),breaks=seq(50,80,5))+
  scale_color_manual("",values=c("#FC5151","#5E5E5E"))+
  scale_shape_manual("",values=c(16,16))+
  geom_text(mapping=aes(x=yr,y=Mean,label=mean.f,col=type),
            position=position_dodge(width=0.4),hjust=-0.4,show.legend=FALSE)+
  theme_complete_bw()
dev.off()


#---------------------------------------
# School performance
#---------------------------------------
vx.y1.o.perf$yr="2014-15"
vx.y2.o.perf$yr="2015-16"
vx.y3.o.perf$yr="2016-17"

vx.graph.perf=rbind(vx.y1.o.perf,vx.y2.o.perf,vx.y3.o.perf)

vx.graph.perf$yr=as.factor(vx.graph.perf$yr)
vx.graph.perf$Mean=vx.graph.perf$Mean*100
vx.graph.perf$mean.f=sprintf("%0.0f",vx.graph.perf$Mean)
vx.graph.perf$type=rep(c("High participation","Low participation"),3)
vx.graph.perf$type=factor(vx.graph.perf$type,levels=c("Low participation","High participation"))

vx.graph.perf$lb=vx.graph.perf$LowerCI*100
vx.graph.perf$ub=vx.graph.perf$UpperCI*100

pdf(file=paste0(plot.dir,"fig-vxcov-perf.pdf"),width=8,height=3)
ggplot(vx.graph.perf,aes(x=yr,y=Mean,group=type))+
  geom_point(aes(shape=type,col=type),
             size=3,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=lb,ymax=ub,col=type),
                position=position_dodge(width=0.4),size=0.8, width=0.15)+
  ylab("Percent vaccinated for influenza")+xlab("")+
  scale_y_continuous(limits=c(50,80),labels=seq(50,80,5),breaks=seq(50,80,5))+
  scale_color_manual("",values=c("#9282FA","#4A2FFA"))+
  scale_shape_manual("",values=c(17,16))+
  geom_text(mapping=aes(x=yr,y=Mean,label=mean.f,col=type),
            position=position_dodge(width=0.4),hjust=-0.4,show.legend=FALSE)+
  theme_complete_bw()
dev.off()

