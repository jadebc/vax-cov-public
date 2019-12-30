########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Create figures showing vaccination coverage by year
# standardized RDs
########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

load(vax_standardized_results_path)

res=data.frame(rbind(res.vx.y1,res.vx.y2,res.vx.y3,res.vx.y4,
    res.vx.y1.edu.dist,res.vx.y2.edu.dist,res.vx.y3.edu.dist,res.vx.y4.edu.dist,
    res.vx.y1.edu.dist.s,res.vx.y2.edu.dist.s,res.vx.y3.edu.dist.s,res.vx.y4.edu.dist.s,
    res.vx.y1.race.dist,res.vx.y2.race.dist,res.vx.y3.race.dist,res.vx.y4.race.dist,
    res.vx.y1.race.dist.s,res.vx.y2.race.dist.s,res.vx.y3.race.dist.s,res.vx.y4.race.dist.s))

res$yr=rep(c("2014-15","2015-16","2016-17","2017-18"),5)
res$yr=as.factor(res$yr)

yrs=4

res$type=c(rep("None",yrs),rep("Education - Whole district",yrs),rep("Education - Whole schools",yrs),
           rep("Race - Whole district",yrs),rep("Race - Whole schools",yrs))
res$type=factor(res$type,levels=c("None","Education - Whole district",
          "Education - Whole schools","Race - Whole district","Race - Whole schools"))

pdf(file=paste0(plot_path,"fig-vxcov-rd-std.pdf"),width=7,height=4)
ggplot(res,aes(x=yr,y=pt.est,group=type))+geom_point(aes(col=type),
      position=position_dodge(width=0.4))+
  geom_errorbar(aes(ymin=lb,ymax=ub,col=type),
      position=position_dodge(width=0.4),width=0.2)+
  scale_y_continuous(limits=c(-0.2,0.2),
      breaks=seq(-0.2,0.2,0.05),
      labels=seq(-0.2,0.2,0.05))+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_complete_bw()+xlab("")+ylab("Percent difference (95% CI)")+
  scale_color_manual(values=c("#677480","#165E8C","#28A0ED","#D17C11","#FCB256"))+
  labs(color="Standardization")
dev.off()