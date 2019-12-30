########################################
# Vaccine coverage survey conducted in March 2017
# Create figures for presentation
# vaccination location by parent edu
########################################

rm(list=ls())

library(ggplot2)

load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov.RData")
load("~/Dropbox/Flu/StFData/2017-2018/Data/Temp/vxcov-import.RData")

source("~/Documents/CRG/flu/vax-cov/2-analysis/0-base-functions.R")
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/theme_complete_bw.R")

plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"

data.y4=data.import
data$dist=as.factor(data$dist)

data$race=as.character(data$race)
data$race[data$race=="Multi"]="Multiple"
data$race=as.factor(data$race)

data$vxloc1415[data$vxloc1415=="Error"]="Error/Missing/Don't know"
data$vxloc1415[data$vxloc1415=="Missing/Don't know"]="Error/Missing/Don't know"
data$vxloc1516[data$vxloc1516=="Error"]="Error/Missing/Don't know"
data$vxloc1516[data$vxloc1516=="Missing/Don't know"]="Error/Missing/Don't know"
data$vxloc1617[data$vxloc1617=="Error"]="Error/Missing/Don't know"
data$vxloc1617[data$vxloc1617=="Missing/Don't know"]="Error/Missing/Don't know"

# create indicators for each vaccine location; denominator
# includes missing/error/don't know
data$doc1415=ifelse(data$vxloc1415=="Doctor/clinic",1,0)
data$doc1516=ifelse(data$vxloc1516=="Doctor/clinic",1,0)
data$doc1617=ifelse(data$vxloc1617=="Doctor/clinic",1,0)

data$school1415=ifelse(data$vxloc1415=="School",1,0)
data$school1516=ifelse(data$vxloc1516=="School",1,0)
data$school1617=ifelse(data$vxloc1617=="School",1,0)

data$other1415=ifelse(data$vxloc1415=="Other",1,0)
data$other1516=ifelse(data$vxloc1516=="Other",1,0)
data$other1617=ifelse(data$vxloc1617=="Other",1,0)

data$error1415=ifelse(data$vxloc1415=="Error/Missing/Don't know",1,0)
data$error1516=ifelse(data$vxloc1516=="Error/Missing/Don't know",1,0)
data$error1617=ifelse(data$vxloc1617=="Error/Missing/Don't know",1,0)

data$vxloc1415=factor(data$vxloc1415,levels=c("Doctor/clinic",
        "School","Other","Error/Missing/Don't know"))
data$vxloc1516=factor(data$vxloc1516,levels=c("Doctor/clinic",
        "School","Other","Error/Missing/Don't know"))
data$vxloc1617=factor(data$vxloc1617,levels=c("Doctor/clinic",
        "School","Other","Error/Missing/Don't know"))

data.y4$dist=as.factor(data.y4$dist)

data.y4$race=as.character(data.y4$race)
data.y4$race[data.y4$race=="Multi"]="Multiple"
data.y4$race=as.factor(data.y4$race)

data.y4$vxloc1718[data.y4$vxloc1718=="Error"]="Error/Missing/Don't know"
data.y4$vxloc1718[data.y4$vxloc1718=="Missing/Don't know"]="Error/Missing/Don't know"

# create indicators for each vaccine location; denominator
# includes missing/error/don't know
data.y4$doc1718=ifelse(data.y4$vxloc1718=="Doctor/clinic",1,0)
data.y4$school1718=ifelse(data.y4$vxloc1718=="School",1,0)
data.y4$other1718=ifelse(data.y4$vxloc1718=="Other",1,0)
data.y4$error1718=ifelse(data.y4$vxloc1718=="Error/Missing/Don't know",1,0)

data.y4$vxloc1718=factor(data.y4$vxloc1718,levels=c("Doctor/clinic",
                                                    "School","Other","Error/Missing/Don't know"))

graph1415.o=as.data.frame(prop.table(table(data$race[data$vx1415==1 & data$dist=="OUSD"],
    data$vxloc1415[data$vx1415==1 & data$dist=="OUSD"]),1))
graph1415.w=as.data.frame(prop.table(table(data$race[data$vx1415==1 & data$dist=="WCCUSD"],
    data$vxloc1415[data$vx1415==1 & data$dist=="WCCUSD"]),1))
colnames(graph1415.o)=c("race","loc","percent")
colnames(graph1415.w)=c("race","loc","percent")
graph1415.o$yr="Yr 1"
graph1415.w$yr="Yr 1"
graph1415.o$dist="OUSD"
graph1415.w$dist="WCCUSD"

graph1516.o=as.data.frame(prop.table(table(data$race[data$vx1516==1 & data$dist=="OUSD"],
    data$vxloc1516[data$vx1516==1 & data$dist=="OUSD"]),1))
graph1516.w=as.data.frame(prop.table(table(data$race[data$vx1516==1 & data$dist=="WCCUSD"],
    data$vxloc1516[data$vx1516==1 & data$dist=="WCCUSD"]),1))
colnames(graph1516.o)=c("race","loc","percent")
colnames(graph1516.w)=c("race","loc","percent")
graph1516.o$yr="Yr 2"
graph1516.w$yr="Yr 2"
graph1516.o$dist="OUSD"
graph1516.w$dist="WCCUSD"

graph1617.o=as.data.frame(prop.table(table(data$race[data$vx1617==1 & data$dist=="OUSD"],
    data$vxloc1617[data$vx1617==1 & data$dist=="OUSD"]),1))
graph1617.w=as.data.frame(prop.table(table(data$race[data$vx1617==1 & data$dist=="WCCUSD"],
    data$vxloc1617[data$vx1617==1 & data$dist=="WCCUSD"]),1))
colnames(graph1617.o)=c("race","loc","percent")
colnames(graph1617.w)=c("race","loc","percent")
graph1617.o$yr="Yr 3"
graph1617.w$yr="Yr 3"
graph1617.o$dist="OUSD"
graph1617.w$dist="WCCUSD"

graph1718.o=as.data.frame(prop.table(table(data.y4$race[data.y4$vx1718==1 & data.y4$dist=="OUSD"],
        data.y4$vxloc1718[data.y4$vx1718==1 & data.y4$dist=="OUSD"]),1))
graph1718.w=as.data.frame(prop.table(table(data.y4$race[data.y4$vx1718==1 & data.y4$dist=="WCCUSD"],
        data.y4$vxloc1718[data.y4$vx1718==1 & data.y4$dist=="WCCUSD"]),1))
colnames(graph1718.o)=c("race","loc","percent")
colnames(graph1718.w)=c("race","loc","percent")
graph1718.o$yr="Yr 4"
graph1718.w$yr="Yr 4"
graph1718.o$dist="OUSD"
graph1718.w$dist="WCCUSD"

graph.all=rbind(graph1415.o,graph1415.w,graph1516.o,graph1516.w,
          graph1617.o,graph1617.w,graph1718.o,graph1718.w)
graph.all$percent=graph.all$percent*100

# too few observations for native american/PI, dropping
graph.all=graph.all[graph.all$race!="Native American",]
graph.all=graph.all[graph.all$race!="Pacific islander",]

graph.all$race=factor(graph.all$race,levels=c("Asian",
    "Black","Latino","White","Multiple","Missing"))

pdf(file=paste0(plot.dir,"fig-vxloc-race-y4.pdf"),width=10,height=4)
ggplot(graph.all,aes(x=yr,y=percent,fill=loc))+
  geom_bar(stat="identity",width=0.5)+
  facet_grid(dist~race)+
  scale_fill_manual("",values=c("#2185c5","#ff9715","#f20253","#677480"))+
  theme_complete_bw()+ylab("Percent")+xlab("")+
  theme(legend.position="bottom")
dev.off()
