########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Estimate vaccination coverage from 2017 survey
########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

data = read.csv(data_path_2017)

#-------------------------------------
# vaccine coverage
#-------------------------------------
# create indicator with 1=vacinated, 0=not vaccinated, error, missing
data$vx1415yn=ifelse(data$vx1415==1,1,0)
data$vx1516yn=ifelse(data$vx1516==1,1,0)
data$vx1617yn=ifelse(data$vx1617==1,1,0)

table(data$vx1415,data$dist,useNA="ifany")
vx.y1.o=as.data.frame(mean_se(Y=data$vx1415yn[data$dist=="OUSD"],
        id=data$matchid[data$dist=="OUSD"]))
vx.y1.w=as.data.frame(mean_se(Y=data$vx1415yn[data$dist=="WCCUSD"],
        id=data$matchid[data$dist=="WCCUSD"]))

table(data$vx1516,data$dist)
vx.y2.o=as.data.frame(mean_se(Y=data$vx1516yn[data$dist=="OUSD"],
        id=data$matchid[data$dist=="OUSD"]))
vx.y2.w=as.data.frame(mean_se(Y=data$vx1516yn[data$dist=="WCCUSD"],
        id=data$matchid[data$dist=="WCCUSD"]))

table(data$vx1617,data$dist)
vx.y3.o=as.data.frame(mean_se(Y=data$vx1617yn[data$dist=="OUSD"],
        id=data$matchid[data$dist=="OUSD"]))
vx.y3.w=as.data.frame(mean_se(Y=data$vx1617yn[data$dist=="WCCUSD"],
        id=data$matchid[data$dist=="WCCUSD"]))

# vax cov by race #-------------------------------------
table(data$vx1415,data$dist,data$race,useNA="ifany")
vx.y1.o.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1415yn[data$dist=="OUSD" & data$race==x],
  id=data$matchid[data$dist=="OUSD" & data$race==x],print=FALSE))))
colnames(vx.y1.o.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y1.w.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1415yn[data$dist=="WCCUSD" & data$race==x],
  id=data$matchid[data$dist=="WCCUSD" & data$race==x],print=FALSE))))
colnames(vx.y1.w.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y2.o.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1516yn[data$dist=="OUSD" & data$race==x],
  id=data$matchid[data$dist=="OUSD" & data$race==x],print=FALSE))))
colnames(vx.y2.o.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y2.w.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1516yn[data$dist=="WCCUSD" & data$race==x],
  id=data$matchid[data$dist=="WCCUSD" & data$race==x],print=FALSE))))
colnames(vx.y2.w.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y3.o.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1617yn[data$dist=="OUSD" & data$race==x],
  id=data$matchid[data$dist=="OUSD" & data$race==x],print=FALSE))))
colnames(vx.y3.o.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y3.w.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1617yn[data$dist=="WCCUSD" & data$race==x],
  id=data$matchid[data$dist=="WCCUSD" & data$race==x],print=FALSE))))
colnames(vx.y3.w.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

# vax cov by education #-------------------------------------
data$edu=droplevels(data$edu)
table(data$vx1415,data$dist,data$edu,useNA="ifany")
vx.y1.o.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1415yn[data$dist=="OUSD" & data$edu==x],
  id=data$matchid[data$dist=="OUSD" & data$edu==x],print=FALSE))))
colnames(vx.y1.o.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y1.w.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1415yn[data$dist=="WCCUSD" & data$edu==x],
  id=data$matchid[data$dist=="WCCUSD" & data$edu==x],print=FALSE))))
colnames(vx.y1.w.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y2.o.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1516yn[data$dist=="OUSD" & data$edu==x],
  id=data$matchid[data$dist=="OUSD" & data$edu==x],print=FALSE))))
colnames(vx.y2.o.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y2.w.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1516yn[data$dist=="WCCUSD" & data$edu==x],
  id=data$matchid[data$dist=="WCCUSD" & data$edu==x],print=FALSE))))
colnames(vx.y2.w.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y3.o.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1617yn[data$dist=="OUSD" & data$edu==x],
  id=data$matchid[data$dist=="OUSD" & data$edu==x],print=FALSE))))
colnames(vx.y3.o.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y3.w.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1617yn[data$dist=="WCCUSD" & data$edu==x],
  id=data$matchid[data$dist=="WCCUSD" & data$edu==x],print=FALSE))))
colnames(vx.y3.w.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

#-------------------------------------
# vaccine type 
#-------------------------------------
# combine error with missing/dk
data$vxtype1415 = as.character(data$vxtype1415)
data$vxtype1516 = as.character(data$vxtype1516)
data$vxtype1617 = as.character(data$vxtype1617)

data$vxtype1415[data$vxtype1415=="Error"]="Error/Missing/Don't know"
data$vxtype1415[data$vxtype1415=="Missing/Don't know"]="Error/Missing/Don't know"
data$vxtype1516[data$vxtype1516=="Error"]="Error/Missing/Don't know"
data$vxtype1516[data$vxtype1516=="Missing/Don't know"]="Error/Missing/Don't know"
data$vxtype1617[data$vxtype1617=="Error"]="Error/Missing/Don't know"
data$vxtype1617[data$vxtype1617=="Missing/Don't know"]="Error/Missing/Don't know"

# create indicators for each type of vaccine; denominator
# includes missing/error/don't know
data$shot1415=ifelse(data$vxtype1415=="Shot",1,0)
data$shot1516=ifelse(data$vxtype1516=="Shot",1,0)
data$shot1617=ifelse(data$vxtype1617=="Shot",1,0)

data$spray1415=ifelse(data$vxtype1415=="Spray",1,0)
data$spray1516=ifelse(data$vxtype1516=="Spray",1,0)
data$spray1617=ifelse(data$vxtype1617=="Spray",1,0)

data$error1415=ifelse(data$vxtype1415=="Error/Missing/Don't know",1,0)
data$error1516=ifelse(data$vxtype1516=="Error/Missing/Don't know",1,0)
data$error1617=ifelse(data$vxtype1617=="Error/Missing/Don't know",1,0)

table(data$vxtype1415[data$vx1415==1],data$dist[data$vx1415==1])
table(data$vxtype1516[data$vx1516==1],data$dist[data$vx1516==1])
table(data$vxtype1617[data$vx1617==1],data$dist[data$vx1617==1])

shot.y1.o=as.data.frame(mean_se(Y=data$shot1415[data$dist=="OUSD" & data$vx1415==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1415==1]))
spray.y1.o=as.data.frame(mean_se(Y=data$spray1415[data$dist=="OUSD" & data$vx1415==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1415==1]))
error.y1.o=as.data.frame(mean_se(Y=data$error1415[data$dist=="OUSD" & data$vx1415==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1415==1]))

shot.y1.w=as.data.frame(mean_se(Y=data$shot1415[data$dist=="WCCUSD" & data$vx1415==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1415==1]))
spray.y1.w=as.data.frame(mean_se(Y=data$spray1415[data$dist=="WCCUSD" & data$vx1415==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1415==1]))
error.y1.w=as.data.frame(mean_se(Y=data$error1415[data$dist=="WCCUSD" & data$vx1415==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1415==1]))

shot.y2.o=as.data.frame(mean_se(Y=data$shot1516[data$dist=="OUSD" & data$vx1516==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1516==1]))
spray.y2.o=as.data.frame(mean_se(Y=data$spray1516[data$dist=="OUSD" & data$vx1516==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1516==1]))
error.y2.o=as.data.frame(mean_se(Y=data$error1516[data$dist=="OUSD" & data$vx1516==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1516==1]))

shot.y2.w=as.data.frame(mean_se(Y=data$shot1516[data$dist=="WCCUSD" & data$vx1516==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1516==1]))
spray.y2.w=as.data.frame(mean_se(Y=data$spray1516[data$dist=="WCCUSD" & data$vx1516==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1516==1]))
error.y2.w=as.data.frame(mean_se(Y=data$error1516[data$dist=="WCCUSD" & data$vx1516==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1516==1]))

shot.y3.o=as.data.frame(mean_se(Y=data$shot1617[data$dist=="OUSD" & data$vx1617==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1617==1]))
spray.y3.o=as.data.frame(mean_se(Y=data$spray1617[data$dist=="OUSD" & data$vx1617==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1617==1]))
error.y3.o=as.data.frame(mean_se(Y=data$error1617[data$dist=="OUSD" & data$vx1617==1],
       id=data$matchid[data$dist=="OUSD" & data$vx1617==1]))

shot.y3.w=as.data.frame(mean_se(Y=data$shot1617[data$dist=="WCCUSD" & data$vx1617==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1617==1]))
spray.y3.w=as.data.frame(mean_se(Y=data$spray1617[data$dist=="WCCUSD" & data$vx1617==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1617==1]))
error.y3.w=as.data.frame(mean_se(Y=data$error1617[data$dist=="WCCUSD" & data$vx1617==1],
       id=data$matchid[data$dist=="WCCUSD" & data$vx1617==1]))

#-------------------------------------
# vaccine type, including non vaccinated
#-------------------------------------
data$vxtype1415comp=data$vxtype1415
data$vxtype1415comp[data$vx1415!=1]="Not vaccinated/Don't know if vax"
data$vxtype1516comp=data$vxtype1516
data$vxtype1516comp[data$vx1516!=1]="Not vaccinated/Don't know if vax"
data$vxtype1617comp=data$vxtype1617
data$vxtype1617comp[data$vx1617!=1]="Not vaccinated/Don't know if vax"

# create indicators for each type of vaccine; denominator
# includes missing/error/don't know
data$shotcomp1415=ifelse(data$vxtype1415comp=="Shot",1,0)
data$shotcomp1516=ifelse(data$vxtype1516comp=="Shot",1,0)
data$shotcomp1617=ifelse(data$vxtype1617comp=="Shot",1,0)

data$spraycomp1415=ifelse(data$vxtype1415comp=="Spray",1,0)
data$spraycomp1516=ifelse(data$vxtype1516comp=="Spray",1,0)
data$spraycomp1617=ifelse(data$vxtype1617comp=="Spray",1,0)

data$novxcomp1415=ifelse(data$vxtype1415comp=="Not vaccinated/Don't know if vax",1,0)
data$novxcomp1516=ifelse(data$vxtype1516comp=="Not vaccinated/Don't know if vax",1,0)
data$novxcomp1617=ifelse(data$vxtype1617comp=="Not vaccinated/Don't know if vax",1,0)

data$errorcomp1415=ifelse(data$vxtype1415comp=="Error/Missing/Don't know",1,0)
data$errorcomp1516=ifelse(data$vxtype1516comp=="Error/Missing/Don't know",1,0)
data$errorcomp1617=ifelse(data$vxtype1617comp=="Error/Missing/Don't know",1,0)

table(data$vxtype1415comp[data$vx1415==1],data$dist[data$vx1415==1])
table(data$vxtype1516comp[data$vx1516==1],data$dist[data$vx1516==1])
table(data$vxtype1617comp[data$vx1617==1],data$dist[data$vx1617==1])

shot.comp.y1.o=as.data.frame(mean_se(Y=data$shotcomp1415[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
spray.comp.y1.o=as.data.frame(mean_se(Y=data$spraycomp1415[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
novx.comp.y1.o=as.data.frame(mean_se(Y=data$novxcomp1415[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
error.comp.y1.o=as.data.frame(mean_se(Y=data$errorcomp1415[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))

shot.comp.y1.w=as.data.frame(mean_se(Y=data$shotcomp1415[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
spray.comp.y1.w=as.data.frame(mean_se(Y=data$spraycomp1415[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
novx.comp.y1.w=as.data.frame(mean_se(Y=data$novxcomp1415[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
error.comp.y1.w=as.data.frame(mean_se(Y=data$errorcomp1415[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))

shot.comp.y2.o=as.data.frame(mean_se(Y=data$shotcomp1516[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
spray.comp.y2.o=as.data.frame(mean_se(Y=data$spraycomp1516[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
novx.comp.y2.o=as.data.frame(mean_se(Y=data$novxcomp1516[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
error.comp.y2.o=as.data.frame(mean_se(Y=data$errorcomp1516[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))

shot.comp.y2.w=as.data.frame(mean_se(Y=data$shotcomp1516[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
spray.comp.y2.w=as.data.frame(mean_se(Y=data$spraycomp1516[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
novx.comp.y2.w=as.data.frame(mean_se(Y=data$novxcomp1516[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
error.comp.y2.w=as.data.frame(mean_se(Y=data$errorcomp1516[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))

shot.comp.y3.o=as.data.frame(mean_se(Y=data$shotcomp1617[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
spray.comp.y3.o=as.data.frame(mean_se(Y=data$spraycomp1617[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
novx.comp.y3.o=as.data.frame(mean_se(Y=data$novxcomp1617[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))
error.comp.y3.o=as.data.frame(mean_se(Y=data$errorcomp1617[data$dist=="OUSD"],
       id=data$matchid[data$dist=="OUSD"]))

shot.comp.y3.w=as.data.frame(mean_se(Y=data$shotcomp1617[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
spray.comp.y3.w=as.data.frame(mean_se(Y=data$spraycomp1617[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
novx.comp.y3.w=as.data.frame(mean_se(Y=data$novxcomp1617[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))
error.comp.y3.w=as.data.frame(mean_se(Y=data$errorcomp1617[data$dist=="WCCUSD"],
       id=data$matchid[data$dist=="WCCUSD"]))

#-------------------------------------
# vaccine location 
#-------------------------------------
# combine error with missing/dk
data$vxloc1415 = as.character(data$vxloc1415)
data$vxloc1516 = as.character(data$vxloc1516)
data$vxloc1617 = as.character(data$vxloc1617)

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

table(data$vxloc1415[data$vx1415==1],data$dist[data$vx1415==1])
table(data$vxloc1516[data$vx1516==1],data$dist[data$vx1516==1])
table(data$vxloc1617[data$vx1617==1],data$dist[data$vx1617==1])

doc.y1.o=as.data.frame(mean_se(Y=data$doc1415[data$dist=="OUSD" & data$vx1415==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1415==1]))
sch.y1.o=as.data.frame(mean_se(Y=data$school1415[data$dist=="OUSD" & data$vx1415==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1415==1]))
oth.y1.o=as.data.frame(mean_se(Y=data$other1415[data$dist=="OUSD" & data$vx1415==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1415==1]))
errorloc.y1.o=as.data.frame(mean_se(Y=data$error1415[data$dist=="OUSD" & data$vx1415==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1415==1]))

doc.y2.o=as.data.frame(mean_se(Y=data$doc1516[data$dist=="OUSD" & data$vx1516==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1516==1]))
sch.y2.o=as.data.frame(mean_se(Y=data$school1516[data$dist=="OUSD" & data$vx1516==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1516==1]))
oth.y2.o=as.data.frame(mean_se(Y=data$other1516[data$dist=="OUSD" & data$vx1516==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1516==1]))
errorloc.y2.o=as.data.frame(mean_se(Y=data$error1516[data$dist=="OUSD" & data$vx1516==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1516==1]))

doc.y3.o=as.data.frame(mean_se(Y=data$doc1617[data$dist=="OUSD" & data$vx1617==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1617==1]))
sch.y3.o=as.data.frame(mean_se(Y=data$school1617[data$dist=="OUSD" & data$vx1617==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1617==1]))
oth.y3.o=as.data.frame(mean_se(Y=data$other1617[data$dist=="OUSD" & data$vx1617==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1617==1]))
errorloc.y3.o=as.data.frame(mean_se(Y=data$error1617[data$dist=="OUSD" & data$vx1617==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1617==1]))

doc.y1.w=as.data.frame(mean_se(Y=data$doc1415[data$dist=="WCCUSD" & data$vx1415==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1415==1]))
sch.y1.w=as.data.frame(mean_se(Y=data$school1415[data$dist=="WCCUSD" & data$vx1415==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1415==1]))
oth.y1.w=as.data.frame(mean_se(Y=data$other1415[data$dist=="WCCUSD" & data$vx1415==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1415==1]))
errorloc.y1.w=as.data.frame(mean_se(Y=data$error1415[data$dist=="WCCUSD" & data$vx1415==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1415==1]))

doc.y2.w=as.data.frame(mean_se(Y=data$doc1516[data$dist=="WCCUSD" & data$vx1516==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1516==1]))
sch.y2.w=as.data.frame(mean_se(Y=data$school1516[data$dist=="WCCUSD" & data$vx1516==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1516==1]))
oth.y2.w=as.data.frame(mean_se(Y=data$other1516[data$dist=="WCCUSD" & data$vx1516==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1516==1]))
errorloc.y2.w=as.data.frame(mean_se(Y=data$error1516[data$dist=="WCCUSD" & data$vx1516==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1516==1]))

doc.y3.w=as.data.frame(mean_se(Y=data$doc1617[data$dist=="WCCUSD" & data$vx1617==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1617==1]))
sch.y3.w=as.data.frame(mean_se(Y=data$school1617[data$dist=="WCCUSD" & data$vx1617==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1617==1]))
oth.y3.w=as.data.frame(mean_se(Y=data$other1617[data$dist=="WCCUSD" & data$vx1617==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1617==1]))
errorloc.y3.w=as.data.frame(mean_se(Y=data$error1617[data$dist=="WCCUSD" & data$vx1617==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1617==1]))

#-------------------------------------
# vaccine location, including non vaccinated
#-------------------------------------
data$vxloc1415comp=as.character(data$vxloc1415)
data$vxloc1415comp[data$vx1415!=1]="Not vaccinated/Don't know if vax"
data$vxloc1516comp=as.character(data$vxloc1516)
data$vxloc1516comp[data$vx1516!=1]="Not vaccinated/Don't know if vax"
data$vxloc1617comp=as.character(data$vxloc1617)
data$vxloc1617comp[data$vx1617!=1]="Not vaccinated/Don't know if vax"

# create indicators for each vaccine location; denominator
# includes missing/error/don't know
data$doccomp1415=ifelse(data$vxloc1415comp=="Doctor/clinic",1,0)
data$doccomp1516=ifelse(data$vxloc1516comp=="Doctor/clinic",1,0)
data$doccomp1617=ifelse(data$vxloc1617comp=="Doctor/clinic",1,0)

data$schoolcomp1415=ifelse(data$vxloc1415comp=="School",1,0)
data$schoolcomp1516=ifelse(data$vxloc1516comp=="School",1,0)
data$schoolcomp1617=ifelse(data$vxloc1617comp=="School",1,0)

data$othercomp1415=ifelse(data$vxloc1415comp=="Other",1,0)
data$othercomp1516=ifelse(data$vxloc1516comp=="Other",1,0)
data$othercomp1617=ifelse(data$vxloc1617comp=="Other",1,0)

data$errorcomp1415=ifelse(data$vxloc1415comp=="Error/Missing/Don't know",1,0)
data$errorcomp1516=ifelse(data$vxloc1516comp=="Error/Missing/Don't know",1,0)
data$errorcomp1617=ifelse(data$vxloc1617comp=="Error/Missing/Don't know",1,0)

data$novxcomp1415=ifelse(data$vxloc1415comp=="Not vaccinated/Don't know if vax",1,0)
data$novxcomp1516=ifelse(data$vxloc1516comp=="Not vaccinated/Don't know if vax",1,0)
data$novxcomp1617=ifelse(data$vxloc1617comp=="Not vaccinated/Don't know if vax",1,0)

table(data$vxloc1415comp,data$dist)
table(data$vxloc1516comp,data$dist)
table(data$vxloc1617comp,data$dist)

doc.comp.y1.o=as.data.frame(mean_se(Y=data$doccomp1415[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
sch.comp.y1.o=as.data.frame(mean_se(Y=data$schoolcomp1415[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
oth.comp.y1.o=as.data.frame(mean_se(Y=data$othercomp1415[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
novxloc.comp.y1.o=as.data.frame(mean_se(Y=data$novxcomp1415[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
errorloc.comp.y1.o=as.data.frame(mean_se(Y=data$errorcomp1415[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))

doc.comp.y2.o=as.data.frame(mean_se(Y=data$doccomp1516[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
sch.comp.y2.o=as.data.frame(mean_se(Y=data$schoolcomp1516[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
oth.comp.y2.o=as.data.frame(mean_se(Y=data$othercomp1516[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
novxloc.comp.y2.o=as.data.frame(mean_se(Y=data$novxcomp1516[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
errorloc.comp.y2.o=as.data.frame(mean_se(Y=data$errorcomp1516[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))

doc.comp.y3.o=as.data.frame(mean_se(Y=data$doccomp1617[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
sch.comp.y3.o=as.data.frame(mean_se(Y=data$schoolcomp1617[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
oth.comp.y3.o=as.data.frame(mean_se(Y=data$othercomp1617[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
novxloc.comp.y3.o=as.data.frame(mean_se(Y=data$novxcomp1617[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
errorloc.comp.y3.o=as.data.frame(mean_se(Y=data$errorcomp1617[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))

doc.comp.y1.w=as.data.frame(mean_se(Y=data$doccomp1415[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
sch.comp.y1.w=as.data.frame(mean_se(Y=data$schoolcomp1415[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
oth.comp.y1.w=as.data.frame(mean_se(Y=data$othercomp1415[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
novxloc.comp.y1.w=as.data.frame(mean_se(Y=data$novxcomp1415[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
errorloc.comp.y1.w=as.data.frame(mean_se(Y=data$errorcomp1415[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))

doc.comp.y2.w=as.data.frame(mean_se(Y=data$doccomp1516[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
sch.comp.y2.w=as.data.frame(mean_se(Y=data$schoolcomp1516[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
oth.comp.y2.w=as.data.frame(mean_se(Y=data$othercomp1516[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
novxloc.comp.y2.w=as.data.frame(mean_se(Y=data$novxcomp1516[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
errorloc.comp.y2.w=as.data.frame(mean_se(Y=data$errorcomp1516[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))

doc.comp.y3.w=as.data.frame(mean_se(Y=data$doccomp1617[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
sch.comp.y3.w=as.data.frame(mean_se(Y=data$schoolcomp1617[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
oth.comp.y3.w=as.data.frame(mean_se(Y=data$othercomp1617[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
novxloc.comp.y3.w=as.data.frame(mean_se(Y=data$novxcomp1617[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
errorloc.comp.y3.w=as.data.frame(mean_se(Y=data$errorcomp1617[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))

# future vaccination
table(data$vxplan,data$dist)
prop.table(table(data$vxplan,data$dist),2)

rm(data, mean_se)

save.image(file=vax_results_2017_path)

