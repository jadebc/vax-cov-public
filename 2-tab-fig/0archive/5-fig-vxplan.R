########################################
# Vaccine coverage survey conducted in March 2017

# Plans to vaccinate in 2017-18
########################################

rm(list=ls())
library(ggplot2)
library(reshape2)

load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov.RData")
source("~/Documents/CRG/flu/vax-cov/2-analysis/0-base-functions.R")
data$dist=as.factor(data$dist)
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/theme_complete_bw.R")

plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"

#-------------------------------------
# vaccine coverage
#-------------------------------------
# create indicator with 1=vacinated, 0=not vaccinated, error, missing
data$vx1415yn=ifelse(data$vx1415==1,1,0)
data$vx1516yn=ifelse(data$vx1516==1,1,0)
data$vx1617yn=ifelse(data$vx1617==1,1,0)

#-------------------------------------
# plan to vaccinate by district
#-------------------------------------
table(data$vxplan,data$dist)
vxplan.all=as.data.frame(prop.table(table(data$vxplan,data$dist),2))
vxplan.all$type="all"
#-------------------------------------
# plan to vaccinate by district and vax yr 3
#-------------------------------------
vxplan.yr3vx=as.data.frame(prop.table(table(data$vxplan[data$vx1617yn==1],data$dist[data$vx1617yn==1]),2))
vxplan.yr3novx=as.data.frame(prop.table(table(data$vxplan[data$vx1617yn==0],data$dist[data$vx1617yn==0]),2))

vxplan.yr3vx$type="Vaccinated in Yr 3"
vxplan.yr3novx$type="Not vaccinated in Yr 3"

graph.vxplan=rbind(vxplan.yr3vx,vxplan.yr3novx)
colnames(graph.vxplan)=c("loc","dist","percent","type")
graph.vxplan=graph.vxplan[graph.vxplan$loc=="Doctor/clinic"|
     graph.vxplan$loc=="No vaccine"|graph.vxplan$loc=="School",]

graph.vxplan$loc=factor(graph.vxplan$loc,levels=c("No vaccine",
    "Doctor/clinic","School"))
graph.vxplan$percent=graph.vxplan$percent*100
graph.vxplan$percent.f=sprintf("%0.0f",graph.vxplan$percent)

pdf(file=paste0(plot.dir,"fig-vxplan.pdf"),width=9,height=3)
ggplot(graph.vxplan,aes(x=loc,y=percent,fill=type))+
  geom_bar(stat="identity",position=position_dodge(width=0.9),color="black",size=0.1)+
  scale_fill_manual("",values=c("#7E849E50","#4062F7"))+
  facet_wrap(~dist)+theme_complete_bw()+
  xlab("Planned vaccination in 2017-18")+ylab("Percent")+
  geom_text(mapping=aes(x=loc,y=percent,label=percent.f),
    position=position_dodge(width=0.9),vjust=-0.2,
    size=3)+scale_y_continuous(limits=c(0,100))
dev.off()

#-------------------------------------
# plan to vaccinate by district and vax location yr 3
#-------------------------------------
prop.table(table(data$vxplan,data$vxloc1617),2)

#-------------------------------------
# always, never, sometimes vaccinators
#-------------------------------------
data$vxplanyn=ifelse(data$vxplan=="School"|data$vxplan=="Doctor/clinic"|
                       data$vxplan=="Other",1,0)

data$vxer=ifelse(data$vx1415==1 & data$vxloc1415=="School" &
                 data$vx1516==1 & data$vxloc1516=="School" &
                   data$vx1617==1 & data$vxloc1617=="School" &
                   data$vxplan=="School","Always\nvaccinator\n(school)","")
data$vxer[data$vx1415==1 & data$vxloc1415=="Doctor/clinic" &
      data$vx1516==1 & data$vxloc1516=="Doctor/clinic" &
      data$vx1617==1 & data$vxloc1617=="Doctor/clinic" &
      data$vxplan=="Doctor/clinic"]="Always\nvaccinator\n(doctor)"
data$vxer[data$vx1415==1 &  data$vx1516==1 & data$vx1617==1 & 
            data$vxplanyn==1 & data$vxer==""]="Always\nvaccinator\n(mixed source)"

data$vxer[data$vx1415==0 & data$vx1516==0 & data$vx1617==0 & 
      data$vxplan=="No vaccine"]="Never vaccinator"

data$vxer[data$vx1415==1 & data$vx1516==0 & data$vx1617==0 & 
            data$vxplan=="No vaccine"]="Discontinuer"
data$vxer[data$vx1415==1 & data$vx1516==1 & data$vx1617==0 & 
            data$vxplan=="No vaccine"]="Discontinuer"
data$vxer[data$vx1415==1 & data$vx1516==1 & data$vx1617==1 & 
            data$vxplan=="No vaccine"]="Discontinuer"

data$vxer[data$vx1415==0 & data$vx1516==0 & data$vx1617==0 & 
            data$vxplanyn==1]="Initiator"
data$vxer[data$vx1415==0 & data$vx1516==0 & data$vx1617==1 & 
            data$vxplanyn==1]="Initiator"
data$vxer[data$vx1415==0 & data$vx1516==1 & data$vx1617==1 & 
            data$vxplanyn==1]="Initiator"

data$vxer[data$vx1415==8 | data$vx1415==9 |
          data$vx1516==8 | data$vx1516==9 |
          data$vx1617==8 | data$vx1617==9 |
          data$vxplan=="Error"| data$vxplan=="Missing"]="Don't know"

data$vxer[data$vxer==""]="Flip flopper"

data$vxer=factor(data$vxer,levels=c("Never vaccinator",
    "Discontinuer","Flip flopper","Initiator",
    "Always\nvaccinator\n(mixed source)","Always\nvaccinator\n(doctor)",
    "Always\nvaccinator\n(school)","Don't know"))

graph=melt(prop.table(table(data$vxer,data$dist),2)*100)
colnames(graph)=c("vxer","dist","percent")
graph$percent.f=sprintf("%0.0f",graph$percent)

pdf(file=paste0(plot.dir,"fig-vxers.pdf"),width=11,height=3)
ggplot(graph,aes(x=vxer,y=percent,fill=dist))+
  geom_bar(stat="identity",position=position_dodge(width=0.9))+
  scale_fill_manual("",values=c("#2185c5","#ff9715"))+
  geom_text(mapping=aes(x=vxer,y=percent,label=percent.f),
            position=position_dodge(width=0.9),vjust=-0.2,
            size=3)+scale_y_continuous(limits=c(0,50))+
  theme_complete_bw()+xlab("")+ylab("Percent")
dev.off()


