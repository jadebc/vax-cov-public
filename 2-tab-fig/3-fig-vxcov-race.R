########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Create figures showing vaccination coverage by race
########################################

rm(list=ls())
# define directories, load libraries
source(here::here("0-config.R"))

load(vax_results_2017_path)
load(vax_results_2018_path)

vx.y1.o.race$yr="2014-15"
vx.y2.o.race$yr="2015-16"
vx.y3.o.race$yr="2016-17"
vx.y4.o.race$yr="2017-18"

vx.y1.w.race$yr="2014-15"
vx.y2.w.race$yr="2015-16"
vx.y3.w.race$yr="2016-17"
vx.y4.w.race$yr="2017-18"

vx.y1.o.race$dist="OUSD"
vx.y2.o.race$dist="OUSD"
vx.y3.o.race$dist="OUSD"
vx.y4.o.race$dist="OUSD"

vx.y1.w.race$dist="WCCUSD"
vx.y2.w.race$dist="WCCUSD"
vx.y3.w.race$dist="WCCUSD"
vx.y4.w.race$dist="WCCUSD"

vx.y1.o.race$race=rownames(vx.y1.o.race)
vx.y2.o.race$race=rownames(vx.y2.o.race)
vx.y3.o.race$race=rownames(vx.y3.o.race)
vx.y4.o.race$race=rownames(vx.y4.o.race)

vx.y1.w.race$race=rownames(vx.y1.w.race)
vx.y2.w.race$race=rownames(vx.y2.w.race)
vx.y3.w.race$race=rownames(vx.y3.w.race)
vx.y4.w.race$race=rownames(vx.y4.w.race)

vx.race=rbind(vx.y1.o.race,vx.y2.o.race,vx.y3.o.race,vx.y4.o.race,
              vx.y1.w.race,vx.y2.w.race,vx.y3.w.race,vx.y4.w.race)

vx.race$yr=as.factor(vx.race$yr)
vx.race$Mean=vx.race$Mean*100
vx.race$Mean.f=sprintf("%0.0f",vx.race$Mean)
vx.race$lower=vx.race$LowerCI*100
vx.race$upper=vx.race$UpperCI*100

# drop group with too few data
vx.race$race[vx.race$race=="Missing"]="Race not reported"
vx.race$race[vx.race$race=="Multi"]="Multiple races"
vx.race$race[vx.race$race=="Asian"]="Asian American"
vx.race$race[vx.race$race=="Black"]="African American"
vx.race$race[vx.race$race=="Pacific islander"]="Pacific Islander"
vx.race$race=factor(vx.race$race, levels = c("African American", "Asian American",
                                             "Latino", "Native American", "Pacific Islander",
                                             "White","Multiple races","Race not reported"))


# manually fixing CI upper bound for native american 
vx.race$upper[vx.race$race=="Native American" & vx.race$yr=="2014-15"] = 85
vx.race$upper[vx.race$race=="Native American" & vx.race$yr=="2015-16"] = 85

pdf(file=paste0(plot_path,"fig-vxcov-race-time.pdf"),width=12,height=6)
ggplot(vx.race,aes(x=yr,y=Mean,group=dist))+
  geom_linerange(aes(ymin=lower,ymax=upper,col=dist),size=.7,
                width=0.15, alpha = 0.7)+
  geom_line(aes(col=dist), size = 1.5)+
  ylab("Percent of students vaccinated for influenza")+xlab("")+
  scale_y_continuous(limits=c(5,85),labels=seq(5,85,10),breaks=seq(5,85,10))+
  scale_color_manual("",values=c("#2185c5","#ff9715"))+
  scale_shape_manual("",values=c(16,17))+
  theme_complete_bw()+
  theme(strip.text.x = element_text(size = 14)) +
  facet_wrap(~race, nrow = 2)+
  theme(legend.position="bottom") 
dev.off()


