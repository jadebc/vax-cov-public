########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Distribution of parent education in 2017 survey
########################################

rm(list=ls())
# define directories, load libraries
source(here::here("0-config.R"))

data = read.csv(data_path_2017)

load(district_demographics_path)

# exclude people who made an error because it's <1%
data=data[data$edu!="Error",]

# exclude people with missing education 
data=data[data$edu!="Missing",]

#relabel edu categories
data$edu=as.character(data$edu)
data$edu[data$edu=="High school"]="High school graduate"
data$edu[data$edu=="Associate/College"]="College graduate"
data$edu[data$edu=="Postgrad"]="Graduate school"
data$edu=as.factor(data$edu)

svy.edu=as.data.frame(prop.table(table(data$edu,data$dist),2)*100)
colnames(svy.edu)=c("edu","dist","per")
svy.edu$type="Survey"
svy.edu$dist = as.character(svy.edu$dist)
svy.edu$dist[svy.edu$dist=="OUSD"] = "Intervention district"
svy.edu$dist[svy.edu$dist=="WCCUSD"] = "Comparison district"
  
dist.edu.l=melt(dist.edu)
dist.edu.s.l=melt(dist.edu.s)
colnames(dist.edu.l)=c("edu","dist","per")
colnames(dist.edu.s.l)=c("edu","dist","per")

dist.edu.l$dist=as.character(dist.edu.l$dist)
dist.edu.l$dist[dist.edu.l$dist=="oak"]="Intervention district"
dist.edu.l$dist[dist.edu.l$dist=="wcc"]="Comparison district"
dist.edu.l$type="Entire district"

dist.edu.s.l$dist=as.character(dist.edu.s.l$dist)
dist.edu.s.l$dist[dist.edu.s.l$dist=="oak"]="Intervention district"
dist.edu.s.l$dist[dist.edu.s.l$dist=="wcc"]="Comparison district"
dist.edu.s.l$type="Schools\nin sample"

edu=rbind(svy.edu,dist.edu.l,dist.edu.s.l)
edu$edu.f=factor(edu$edu,levels=c("Less than high school","High school graduate",
                                  "College graduate","Graduate school"))

edu$per.f=sprintf("%0.0f",edu$per)

edu=edu[order(edu$type,edu$dist,edu$edu.f),]

edu$per.f[edu$dist=="Comparison district" & edu$edu=="Graduate school"]=""

edu$printper=c(87,58,23,7,
               87,57,22,5,
               
               87,56,18,5,
               86,58,24,5,
               
               93,55,28,9,
               90,57,20,5)

pdf(file=paste0(plot_path,"fig-vxcov-std-edu.pdf"),width=8,height=4)
ggplot(edu,aes(y=per,x=type,fill=edu.f))+geom_bar(stat="identity",width=.7)+
  theme_complete_bw()+ylab("Percent")+xlab("")+
  geom_text(mapping=aes(label=per.f,y=printper),size=3,show.legend=FALSE)+
  facet_grid(~dist)+theme(legend.title=element_blank())+
  scale_fill_manual("",values=c("#79BF3F","#2185c5","#ff9715","#f20253")) +
  theme(
    strip.text.x = element_text(size=14)
  )
dev.off()


