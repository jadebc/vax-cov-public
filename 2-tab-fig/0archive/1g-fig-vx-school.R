########################################
# Vaccine coverage survey conducted in March 2017
# Calculate mean difference in vax stratified by race
########################################

library(ggplot2)
library(reshape2)
load(vax_school_results_path)

vx.y1.school$yr="2014-15"
vx.y2.school$yr="2015-16"
vx.y3.school$yr="2016-17"

slope=merge(vx.y1.school[,c("schoolname","dist","Mean")],
              vx.y2.school[,c("schoolname","dist","Mean")],by=c("schoolname","dist"))
slope=merge(slope,vx.y3.school[,c("schoolname","dist","Mean")],
              by=c("schoolname","dist"))
colnames(slope)[3:5]=c("Meany1","Meany2","Meany3")
slope$slope=0
slope$slope[(slope$Meany1>slope$Meany2) & 
              (slope$Meany2>slope$Meany3)]=-1
slope$slope[(slope$Meany1<slope$Meany2) & 
              (slope$Meany2<slope$Meany3)]=1
slope$slope[(slope$Meany1<slope$Meany2) & 
              (slope$Meany2==slope$Meany3)]=1
slope$slope[(slope$Meany1==slope$Meany2) & 
              (slope$Meany2<slope$Meany3)]=1

graphdf=rbind(vx.y1.school,vx.y2.school,vx.y3.school)
graphdf=merge(graphdf,slope[,c("schoolname","slope")],by="schoolname")
graphdf$slope=as.factor(graphdf$slope)
graphdf$yr=as.factor(graphdf$yr)
graphdf$Mean=graphdf$Mean*100

pdf(file=paste0(plot_path,"fig-school-slope.pdf"),width=7.5,height=4)
ggplot(graphdf,aes(x=yr,y=Mean,group=schoolname))+
  geom_point(aes(col=slope),alpha=0.7,size=2)+
  geom_line(aes(col=slope),alpha=0.7)+
  scale_color_manual(values=c("red","#737373","#069653"),guide=FALSE)+
  theme_complete_bw()+facet_wrap(~dist)+
  ylab("Percent")+xlab("Year")+
  scale_y_continuous(limits=c(30,100))
dev.off()