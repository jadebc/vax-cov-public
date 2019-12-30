########################################
# Vaccine coverage survey conducted in March 2017
# Create table vaccination location by student race

# stratify by school distress and performance
########################################

rm(list=ls())

load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov.RData")
source("~/Documents/CRG/flu/vax-cov/2-analysis/0-base-functions.R")
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/theme_complete_bw.R")

tab.dir="~/Dropbox/Flu/StFData/Vax cov/Tables/"

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
data$distress=as.factor(ifelse(data$sdi<8.5,"Low distress","High distress"))

tab1415.sdi=as.data.frame(prop.table(table(data$distress[data$vx1415==1 & data$dist=="OUSD"],
      data$vxloc1415[data$vx1415==1 & data$dist=="OUSD"]),1))
colnames(tab1415.sdi)=c("distress","loc","percent")
tab1415.sdi.w=reshape(tab1415.sdi, idvar = c("loc"), timevar = c("distress"), 
             direction = "wide")
tab1415.sdi.w$yr="2014-15"

tab1516.sdi=as.data.frame(prop.table(table(data$distress[data$vx1516==1 & data$dist=="OUSD"],
       data$vxloc1516[data$vx1516==1 & data$dist=="OUSD"]),1))
colnames(tab1516.sdi)=c("distress","loc","percent")
tab1516.sdi.w=reshape(tab1516.sdi, idvar = c("loc"), timevar = c("distress"), 
                      direction = "wide")
tab1516.sdi.w$yr="2015-16"

tab1617.sdi=as.data.frame(prop.table(table(data$distress[data$vx1617==1 & data$dist=="OUSD"],
        data$vxloc1617[data$vx1617==1 & data$dist=="OUSD"]),1))
colnames(tab1617.sdi)=c("distress","loc","percent")
tab1617.sdi.w=reshape(tab1617.sdi, idvar = c("loc"), timevar = c("distress"), 
                      direction = "wide")
tab1617.sdi.w$yr="2016-17"

tab.sdi=rbind(tab1415.sdi.w,tab1516.sdi.w,tab1617.sdi.w)
tab.sdi=tab.sdi[,c(1,4,2:3)]
tab.sdi[,3]=sprintf("%0.0f",tab.sdi[,3]*100)
tab.sdi[,4]=sprintf("%0.0f",tab.sdi[,4]*100)

tab.sdi=tab.sdi[tab.sdi$loc=="Doctor/clinic"|tab.sdi$loc=="School",]
tab.sdi=tab.sdi[,c(2,1,3,4)]

#-------------------------------------
# vaccine coverage by school performance
#-------------------------------------
med2014=quantile(data$par2014[data$dist=="OUSD"],probs=0.5)
med2015=quantile(data$par2015[data$dist=="OUSD"],probs=0.5)
med2016=quantile(data$par2016[data$dist=="OUSD"],probs=0.5)

data$perf2014=as.factor(ifelse(data$par2014<med2014,"Low participation","High participation"))
data$perf2015=as.factor(ifelse(data$par2015<med2015,"Low participation","High participation"))
data$perf2016=as.factor(ifelse(data$par2016<med2016,"Low participation","High participation"))

tab1415.perf=as.data.frame(prop.table(table(data$perf2014[data$vx1415==1 & data$dist=="OUSD"],
     data$vxloc1415[data$vx1415==1 & data$dist=="OUSD"]),1))
colnames(tab1415.perf)=c("perf","loc","percent")
tab1415.perf.w=reshape(tab1415.perf, idvar = c("loc"), timevar = c("perf"), 
                      direction = "wide")
tab1415.perf.w$yr="2014-15"

tab1516.perf=as.data.frame(prop.table(table(data$perf2015[data$vx1516==1 & data$dist=="OUSD"],
    data$vxloc1516[data$vx1516==1 & data$dist=="OUSD"]),1))
colnames(tab1516.perf)=c("perf","loc","percent")
tab1516.perf.w=reshape(tab1516.perf, idvar = c("loc"), timevar = c("perf"), 
                      direction = "wide")
tab1516.perf.w$yr="2015-16"

tab1617.perf=as.data.frame(prop.table(table(data$perf2016[data$vx1617==1 & data$dist=="OUSD"],
      data$vxloc1617[data$vx1617==1 & data$dist=="OUSD"]),1))
colnames(tab1617.perf)=c("perf","loc","percent")
tab1617.perf.w=reshape(tab1617.perf, idvar = c("loc"), timevar = c("perf"), 
                      direction = "wide")
tab1617.perf.w$yr="2016-17"

tab.perf=rbind(tab1415.perf.w,tab1516.perf.w,tab1617.perf.w)
tab.perf=tab.perf[,c(1,4,2:3)]
tab.perf[,3]=sprintf("%0.0f",tab.perf[,3]*100)
tab.perf[,4]=sprintf("%0.0f",tab.perf[,4]*100)

tab.perf=tab.perf[tab.perf$loc=="Doctor/clinic"|tab.perf$loc=="School",]
tab.perf=tab.perf[,c(2,1,4,3)]

save(tab.sdi,tab.perf,file=paste0(tab.dir,"tab-loc-perf-sdi.RData"))
