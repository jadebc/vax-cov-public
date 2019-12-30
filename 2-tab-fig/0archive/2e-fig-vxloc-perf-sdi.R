########################################
# Vaccine coverage survey conducted in March 2017
# figure of vaccination location by student race

# stratify by school distress and performance
########################################

rm(list=ls())

library(ggplot2)

load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov.RData")
source("~/Documents/CRG/flu/vax-cov/2-analysis/0-base-functions.R")
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/theme_complete_bw.R")

plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"

data$dist=as.factor(data$dist)

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

#-------------------------------------
# vaccine coverage by school distress
#-------------------------------------
data$distress=as.factor(ifelse(data$sdi<8.5,"Low distress","Distress"))

graph1415.sdi=as.data.frame(prop.table(table(data$distress[data$vx1415==1 & data$dist=="OUSD"],
     data$vxloc1415[data$vx1415==1 & data$dist=="OUSD"]),1))
colnames(graph1415.sdi)=c("distress","loc","percent")
graph1415.sdi$yr="Yr 1"
graph1415.sdi$dist="OUSD"

graph1516.sdi=as.data.frame(prop.table(table(data$distress[data$vx1516==1 & data$dist=="OUSD"],
    data$vxloc1516[data$vx1516==1 & data$dist=="OUSD"]),1))
colnames(graph1516.sdi)=c("distress","loc","percent")
graph1516.sdi$yr="Yr 2"
graph1516.sdi$dist="OUSD"

graph1617.sdi=as.data.frame(prop.table(table(data$distress[data$vx1617==1 & data$dist=="OUSD"],
     data$vxloc1617[data$vx1617==1 & data$dist=="OUSD"]),1))
colnames(graph1617.sdi)=c("distress","loc","percent")
graph1617.sdi$yr="Yr 3"
graph1617.sdi$dist="OUSD"

graph.sdi=rbind(graph1415.sdi,graph1516.sdi,graph1617.sdi)
graph.sdi$percent=graph.sdi$percent*100

pdf(file=paste0(plot.dir,"fig-vxloc-sdi.pdf"),width=8,height=4)
ggplot(graph.sdi,aes(x=distress,y=percent,fill=loc))+
  geom_bar(stat="identity",width=0.5)+
  facet_grid(dist~yr)+
  scale_fill_manual("",values=c("#2185c5","#ff9715","#f20253","#677480"))+
  theme_complete_bw()+ylab("Percent")+xlab("")+
  theme(legend.position="bottom")
dev.off()



#-------------------------------------
# vaccine coverage by school performance
#-------------------------------------
med2014=quantile(data$par2014[data$dist=="OUSD"],probs=0.5)
med2015=quantile(data$par2015[data$dist=="OUSD"],probs=0.5)
med2016=quantile(data$par2016[data$dist=="OUSD"],probs=0.5)

data$perf2014=as.factor(ifelse(data$par2014<med2014,"Low participation","High participation"))
data$perf2015=as.factor(ifelse(data$par2015<med2015,"Low participation","High participation"))
data$perf2016=as.factor(ifelse(data$par2016<med2016,"Low participation","High participation"))

graph1415.perf=as.data.frame(prop.table(table(data$perf2014[data$vx1415==1 & data$dist=="OUSD"],
    data$vxloc1415[data$vx1415==1 & data$dist=="OUSD"]),1))
colnames(graph1415.perf)=c("perf","loc","percent")
graph1415.perf$yr="Yr 1"
graph1415.perf$dist="OUSD"

graph1516.perf=as.data.frame(prop.table(table(data$perf2015[data$vx1516==1 & data$dist=="OUSD"],
    data$vxloc1516[data$vx1516==1 & data$dist=="OUSD"]),1))
colnames(graph1516.perf)=c("perf","loc","percent")
graph1516.perf$yr="Yr 2"
graph1516.perf$dist="OUSD"

graph1617.perf=as.data.frame(prop.table(table(data$perf2016[data$vx1617==1 & data$dist=="OUSD"],
    data$vxloc1617[data$vx1617==1 & data$dist=="OUSD"]),1))
colnames(graph1617.perf)=c("perf","loc","percent")
graph1617.perf$yr="Yr 3"
graph1617.perf$dist="OUSD"

graph.perf=rbind(graph1415.perf,graph1516.perf,graph1617.perf)
graph.perf$percent=graph.perf$percent*100
graph.perf$perf=factor(graph.perf$perf,
      levels=c("Low participation","High participation"))

pdf(file=paste0(plot.dir,"fig-vxloc-perf.pdf"),width=10,height=4)
ggplot(graph.perf,aes(x=perf,y=percent,fill=loc))+
  geom_bar(stat="identity",width=0.5)+
  facet_grid(dist~yr)+
  scale_fill_manual("",values=c("#2185c5","#ff9715","#f20253","#677480"))+
  theme_complete_bw()+ylab("Percent")+xlab("")+
  theme(legend.position="bottom")
dev.off()
