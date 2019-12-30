########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Create figures showing vaccination coverage by education
########################################

rm(list=ls())
# define directories, load libraries
source(here::here("0-config.R"))

load(vax_results_2017_path)
load(vax_results_2018_path)

vx.y1.o.edu$yr="2014-15"
vx.y2.o.edu$yr="2015-16"
vx.y3.o.edu$yr="2016-17"
vx.y4.o.edu$yr="2017-18"

vx.y1.w.edu$yr="2014-15"
vx.y2.w.edu$yr="2015-16"
vx.y3.w.edu$yr="2016-17"
vx.y4.w.edu$yr="2017-18"

vx.y1.o.edu$dist="OUSD"
vx.y2.o.edu$dist="OUSD"
vx.y3.o.edu$dist="OUSD"
vx.y4.o.edu$dist="OUSD"

vx.y1.w.edu$dist="WCCUSD"
vx.y2.w.edu$dist="WCCUSD"
vx.y3.w.edu$dist="WCCUSD"
vx.y4.w.edu$dist="WCCUSD"

vx.y1.o.edu$edu=rownames(vx.y1.o.edu)
vx.y2.o.edu$edu=rownames(vx.y2.o.edu)
vx.y3.o.edu$edu=rownames(vx.y3.o.edu)
vx.y4.o.edu$edu=rownames(vx.y4.o.edu)

vx.y1.w.edu$edu=rownames(vx.y1.w.edu)
vx.y2.w.edu$edu=rownames(vx.y2.w.edu)
vx.y3.w.edu$edu=rownames(vx.y3.w.edu)
vx.y4.w.edu$edu=rownames(vx.y4.w.edu)

vx.edu=rbind(vx.y1.o.edu,vx.y2.o.edu,vx.y3.o.edu,vx.y4.o.edu,
              vx.y1.w.edu,vx.y2.w.edu,vx.y3.w.edu,vx.y4.w.edu)

vx.edu$yr=as.factor(vx.edu$yr)
vx.edu$Mean=vx.edu$Mean*100
vx.edu$Mean.f=sprintf("%0.0f",vx.edu$Mean)
vx.edu$lower=vx.edu$LowerCI*100
vx.edu$upper=vx.edu$UpperCI*100

# drop group with too few data
vx.edu=vx.edu[!vx.edu$edu %in% c("Error"),]
vx.edu=vx.edu[!vx.edu$edu %in% c("Missing"),]

vx.edu$edu=factor(vx.edu$edu,levels=c("Less than high school",
    "High school","Associate/College","Postgrad"))
# levels(vx.edu$edu)[1]="Less than\nhigh school"
# levels(vx.edu$edu)[3]="Associate/\nCollege degree"

# pdf(file=paste0(plot_path,"fig-vxcov-edu-y4.pdf"),width=15,height=3)
# ggplot(vx.edu,aes(x=edu,y=Mean,group=dist))+
#   geom_point(aes(shape=dist,col=dist),position=position_dodge(width=0.5),size=2)+
#   geom_errorbar(aes(ymin=lower,ymax=upper,col=dist),
#                 position=position_dodge(width=0.5), width=0.15,size=.7)+
#   ylab("Percent")+xlab("")+
#   scale_y_continuous(limits=c(40,90),labels=seq(40,90,5),breaks=seq(40,90,5))+
#   scale_color_manual("",values=c("#2185c5","#ff9715"))+
#   scale_shape_manual("",values=c(16,17))+
#   geom_text(mapping=aes(x=edu,y=Mean,label=Mean.f,col=dist),
#             position=position_dodge(width=0.5),hjust=-0.4,show.legend=FALSE,size=2.5)+
#   theme_complete_bw()+facet_grid(~yr)
# dev.off()


# levels(vx.edu$edu)[1]="Less than high school"
# levels(vx.edu$edu)[3]="Associate/College degree"

pdf(file=paste0(plot_path,"fig-vxcov-edu-time.pdf"),width=13,height=4)
ggplot(vx.edu,aes(x=yr,y=Mean,group=dist))+
  geom_linerange(aes(ymin=lower,ymax=upper,col=dist),size=.7, alpha = 0.7)+
  geom_line(aes(col=dist), size = 1.5)+
  ylab("Percent of students vaccinated for influenza")+xlab("")+
  scale_y_continuous(limits=c(45,80),labels=seq(45,80,5),breaks=seq(45,80,5))+
  scale_color_manual("",values=c("#2185c5","#ff9715"))+
  scale_shape_manual("",values=c(16,17))+
  facet_grid(~edu)+
  theme_complete_bw()+
  theme(strip.text.x = element_text(size = 14)) +
  theme(legend.position="bottom")
dev.off()



