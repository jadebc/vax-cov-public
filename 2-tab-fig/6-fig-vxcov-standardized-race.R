########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Distribution of student race in 2017 survey
########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

data = read.csv(data_path_2017)

load(district_demographics_path)

#relabel edu categories
data$race=as.character(data$race)
data$race[data$race=="Black"]="African American"
data$race[data$race=="Multi"]="Multiple"
data$race[data$race=="Missing"]="Not reported"
data$race=as.factor(data$race)
svy.race=as.data.frame(prop.table(table(data$race,data$dist),2)*100)
colnames(svy.race)=c("race","dist","per")
svy.race$type="Survey"
svy.race$dist = as.character(svy.race$dist)
svy.race$dist[svy.race$dist=="OUSD"] = "Intervention district"
svy.race$dist[svy.race$dist=="WCCUSD"] = "Comparison district"

dist.race.l=melt(dist.race)
dist.race.s.l=melt(dist.race.s)
colnames(dist.race.l)=c("race","dist","per")
colnames(dist.race.s.l)=c("race","dist","per")

dist.race.l$dist=as.character(dist.race.l$dist)
dist.race.l$dist[dist.race.l$dist=="oak"]="Intervention district"
dist.race.l$dist[dist.race.l$dist=="wcc"]="Comparison district"
dist.race.l$type="Entire district"

dist.race.s.l$dist=as.character(dist.race.s.l$dist)
dist.race.s.l$dist[dist.race.s.l$dist=="oak"]="Intervention district"
dist.race.s.l$dist[dist.race.s.l$dist=="wcc"]="Comparison district"
dist.race.s.l$type="Schools\nin sample"

race=rbind(svy.race,dist.race.l,dist.race.s.l)
race$race[race$race=="Pacific islander"]="Pacific Islander"
race$race.f=factor(race$race,levels=c("Latino","African American","Asian",
      "White","Multiple","Native American","Pacific Islander","Not reported"))

race$per.f=sprintf("%0.0f",race$per)

race=race[order(race$type,race$dist,race$race.f),]

race$printper=c(
  # Comparison, entire district
  75,38,22,8,0,0,0,0,
  # Intervention, entire district
  78,44,25,13,0,0,0,0,
  # Comparison, schools in sample
  74,37,18,6,0,0,0,0,
  # Intervention, schools in sample
  78,44,18,9,0,0,0,0,
  # Comparison, surveys
  75,44,30,19,9,0,0,0,
  # Intervention, surveys
  78,50,31,17,8,0,0,0)

race$per.f[race$race=="Native American"]=""
race$per.f[race$race=="Pacific Islander"]=""
race$per.f[race$race=="Not reported"]=""
race$per.f[race$race=="Multiple" & race$type=="Entire district"]=""
race$per.f[race$race=="Multiple" & race$type=="Schools\nin sample"]=""

pdf(file=paste0(plot_path, "fig-vxcov-std-race.pdf"),width=8,height=4)
ggplot(race,aes(y=per,x=type,fill=race.f))+geom_bar(stat="identity",width=.7)+
  theme_complete_bw()+ylab("Percent")+xlab("")+
  geom_text(mapping=aes(label=per.f,y=printper),size=3,show.legend=FALSE)+
  facet_grid(~dist)+theme(legend.title=element_blank()) +
  theme(
    strip.text.x = element_text(size=14)
  )
dev.off()


