########################################
# Vaccine coverage survey conducted in March 2017
# Create figures for presentation
# vaccination coverage by year
########################################

rm(list=ls())
# define directories, load libraries
source(here::here("0-config.R"))

load(vax_results_2017_path)
load(vax_standardized_results_path)

vx.y1.o$dist="OUSD"
vx.y1.o$yr="2014-15"
vx.y1.w$dist="WCCUSD"
vx.y1.w$yr="2014-15"

vx.y2.o$dist="OUSD"
vx.y2.o$yr="2015-16"
vx.y2.w$dist="WCCUSD"
vx.y2.w$yr="2015-16"

vx.y3.o$dist="OUSD"
vx.y3.o$yr="2016-17"
vx.y3.w$dist="WCCUSD"
vx.y3.w$yr="2016-17"

pred=data.frame(rbind(pred.vx.y1.edu.dist,pred.vx.y2.edu.dist,pred.vx.y3.edu.dist,
  pred.vx.y1.edu.dist.s,pred.vx.y2.edu.dist.s,pred.vx.y3.edu.dist.s,
  pred.vx.y1.race.dist,pred.vx.y2.race.dist,pred.vx.y3.race.dist,
  pred.vx.y1.race.dist.s,pred.vx.y2.race.dist.s,pred.vx.y3.race.dist.s))
pred$yr=rep(c("2014-15","2014-15","2015-16","2015-16","2016-17","2016-17"),4)
pred$type=c(rep("Education - Whole district",6),rep("Education - Whole schools",6),
      rep("Race - Whole district",6),rep("Race - Whole schools",6))

vx.graph=rbind(vx.y1.o,vx.y1.w,vx.y2.o,vx.y2.w,vx.y3.o,vx.y3.w)
vx.graph=vx.graph[,c("Mean","Lower 95%CI","Upper 95%CI","dist","yr")]
colnames(vx.graph)=c("mean","lb","ub","dist","yr")
vx.graph$type="None"

vx.graph=rbind(vx.graph,pred)

vx.graph$yr=as.factor(vx.graph$yr)
vx.graph$mean=vx.graph$mean*100
vx.graph$mean.f=sprintf("%0.0f",vx.graph$mean)

vx.graph$lb=vx.graph$lb*100
vx.graph$ub=vx.graph$ub*100

vx.graph$type=factor(vx.graph$type,levels=c("None","Education - Whole district",
                                  "Education - Whole schools","Race - Whole district","Race - Whole schools"))

pdf(file=paste0(plot_path,"fig-vxcov.pdf"),width=7,height=5)
ggplot(vx.graph[vx.graph$type=="None",],aes(x=yr,y=mean,group=dist))+
  geom_point(aes(shape=dist,col=dist),size=3,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=lb,ymax=ub,col=dist),
                position=position_dodge(width=0.4),size=0.8, width=0.15)+
  ylab("Percent")+xlab("")+
  scale_y_continuous(limits=c(50,80),labels=seq(50,80,5),breaks=seq(50,80,5))+
  scale_color_manual("",values=c("#2185c5","#ff9715"))+
  scale_shape_manual("",values=c(16,17))+
  geom_text(mapping=aes(x=yr,y=mean,label=mean.f,col=dist),
            position=position_dodge(width=0.4),hjust=-0.4,show.legend=FALSE)+
  theme_complete_bw()
dev.off()

# figure for surveillance paper
pdf(file=paste0(plot_path,"fig-vxcov-bluegreen.pdf"),width=7,height=4)
ggplot(vx.graph[vx.graph$type=="None",],aes(x=yr,y=mean,group=dist))+
  geom_point(aes(shape=dist,col=dist),size=3,position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=lb,ymax=ub,col=dist),
                position=position_dodge(width=0.4),size=0.8, width=0.15)+
  ylab("Percent")+xlab("")+
  scale_y_continuous(limits=c(50,80),labels=seq(50,80,5),breaks=seq(50,80,5))+
  scale_color_manual("District",values=c("#0066cc","#009933"))+
  scale_shape_manual("District",values=c(16,17))+
  geom_text(mapping=aes(x=yr,y=mean,label=mean.f,col=dist),
            position=position_dodge(width=0.4),hjust=-0.4,show.legend=FALSE)+
  theme_complete_bw()
dev.off()


pdf(file=paste0(plot_path,"fig-vxcov-std.pdf"),width=8,height=4)
ggplot(vx.graph,aes(x=yr,y=mean,group=type))+
  geom_point(aes(col=type),size=2,position=position_dodge(width=0.9))+
  geom_errorbar(aes(ymin=lb,ymax=ub,col=type),
                position=position_dodge(width=0.9),size=0.8, width=0.5)+
  ylab("Percent")+xlab("")+
  scale_y_continuous(limits=c(50,80),labels=seq(50,80,5),breaks=seq(50,80,5))+
  scale_color_manual("",values=c("#677480","#165E8C","#28A0ED","#D17C11","#FCB256"))+
  theme_complete_bw()+facet_grid(~dist)
dev.off()

