########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Estimate vaccination coverage from 2018 survey
########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

data = read.csv(data_path_2018)

#-------------------------------------
# vaccine coverage
#-------------------------------------
# create indicator with 1=vacinated, 0=not vaccinated, error, missing
data$vx1516_18yn=ifelse(data$vx1516_18==1,1,0)
data$vx1617_18yn=ifelse(data$vx1617_18==1,1,0)
data$vx1718yn=ifelse(data$vx1718==1,1,0)

table(data$vx1516_18,data$dist,useNA="ifany")
vx.y2_18.o=as.data.frame(mean_se(Y=data$vx1516_18yn[data$dist=="OUSD"],
        id=data$matchid[data$dist=="OUSD"]))
vx.y2_18.w=as.data.frame(mean_se(Y=data$vx1516_18yn[data$dist=="WCCUSD"],
        id=data$matchid[data$dist=="WCCUSD"]))

table(data$vx1617_18,data$dist)
vx.y3_18.o=as.data.frame(mean_se(Y=data$vx1617_18yn[data$dist=="OUSD"],
        id=data$matchid[data$dist=="OUSD"]))
vx.y3_18.w=as.data.frame(mean_se(Y=data$vx1617_18yn[data$dist=="WCCUSD"],
        id=data$matchid[data$dist=="WCCUSD"]))

table(data$vx1718,data$dist)
vx.y4.o=as.data.frame(mean_se(Y=data$vx1718yn[data$dist=="OUSD"],
        id=data$matchid[data$dist=="OUSD"]))
vx.y4.w=as.data.frame(mean_se(Y=data$vx1718yn[data$dist=="WCCUSD"],
        id=data$matchid[data$dist=="WCCUSD"]))

# vax cov by race #-------------------------------------
table(data$vx1516_18,data$dist,data$race,useNA="ifany")
vx.y2_18.o.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1516_18yn[data$dist=="OUSD" & data$race==x],
  id=data$matchid[data$dist=="OUSD" & data$race==x],print=FALSE))))
colnames(vx.y2_18.o.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y2_18.w.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1516_18yn[data$dist=="WCCUSD" & data$race==x],
  id=data$matchid[data$dist=="WCCUSD" & data$race==x],print=FALSE))))
colnames(vx.y2_18.w.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y3_18.o.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1617_18yn[data$dist=="OUSD" & data$race==x],
  id=data$matchid[data$dist=="OUSD" & data$race==x],print=FALSE))))
colnames(vx.y3_18.o.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y3_18.w.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1617_18yn[data$dist=="WCCUSD" & data$race==x],
  id=data$matchid[data$dist=="WCCUSD" & data$race==x],print=FALSE))))
colnames(vx.y3_18.w.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y4.o.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1718yn[data$dist=="OUSD" & data$race==x],
  id=data$matchid[data$dist=="OUSD" & data$race==x],print=FALSE))))
colnames(vx.y4.o.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y4.w.race=as.data.frame(t(sapply(levels(data$race), function(x) mean_se(
  Y=data$vx1718yn[data$dist=="WCCUSD" & data$race==x],
  id=data$matchid[data$dist=="WCCUSD" & data$race==x],print=FALSE))))
colnames(vx.y4.w.race)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

# vax cov by education #-------------------------------------
data$edu=droplevels(data$edu)
table(data$vx1516_18,data$dist,data$edu,useNA="ifany")
vx.y2_18.o.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1516_18yn[data$dist=="OUSD" & data$edu==x],
  id=data$matchid[data$dist=="OUSD" & data$edu==x],print=FALSE))))
colnames(vx.y2_18.o.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y2_18.w.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1516_18yn[data$dist=="WCCUSD" & data$edu==x],
  id=data$matchid[data$dist=="WCCUSD" & data$edu==x],print=FALSE))))
colnames(vx.y2_18.w.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y3_18.o.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1617_18yn[data$dist=="OUSD" & data$edu==x],
  id=data$matchid[data$dist=="OUSD" & data$edu==x],print=FALSE))))
colnames(vx.y3_18.o.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y3_18.w.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1617_18yn[data$dist=="WCCUSD" & data$edu==x],
  id=data$matchid[data$dist=="WCCUSD" & data$edu==x],print=FALSE))))
colnames(vx.y3_18.w.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y4.o.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1718yn[data$dist=="OUSD" & data$edu==x],
  id=data$matchid[data$dist=="OUSD" & data$edu==x],print=FALSE))))
colnames(vx.y4.o.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

vx.y4.w.edu=as.data.frame(t(sapply(levels(data$edu), function(x) mean_se(
  Y=data$vx1718yn[data$dist=="WCCUSD" & data$edu==x],
  id=data$matchid[data$dist=="WCCUSD" & data$edu==x],print=FALSE))))
colnames(vx.y4.w.edu)=c("N","Mean","SD","Robust SE","LowerCI","UpperCI")

#-------------------------------------
# vaccine type
#-------------------------------------
# combine error with missing/dk
data <- data %>%
  mutate(vxtype1718 = case_when(
    vx1718==1 ~ "Shot",
    vx1718==9 ~ "Error/Missing/Don't know"
  ))

# create indicators for each type of vaccine; denominator
# includes missing/error/don't know
data$shot1718=ifelse(data$vxtype1718=="Shot",1,0)
data$error1718=ifelse(data$vxtype1718=="Error/Missing/Don't know",1,0)

table(data$vxtype1718[data$vx1718==1],data$dist[data$vx1718==1])

shot.y4.o=as.data.frame(mean_se(Y=data$shot1718[data$dist=="OUSD" & data$vx1718==1],
                                id=data$matchid[data$dist=="OUSD" & data$vx1718==1]))
error.y4.o=as.data.frame(mean_se(Y=data$error1718[data$dist=="OUSD" & data$vx1718==1],
                                 id=data$matchid[data$dist=="OUSD" & data$vx1718==1]))

shot.y4.w=as.data.frame(mean_se(Y=data$shot1718[data$dist=="WCCUSD" & data$vx1718==1],
                                id=data$matchid[data$dist=="WCCUSD" & data$vx1718==1]))
error.y4.w=as.data.frame(mean_se(Y=data$error1718[data$dist=="WCCUSD" & data$vx1718==1],
                                 id=data$matchid[data$dist=="WCCUSD" & data$vx1718==1]))

#-------------------------------------
# vaccine type, including non vaccinated
#-------------------------------------
data <- data %>%
  mutate(vxtype1718comp = case_when(
    vx1718==1 ~ "Shot",
    vx1718==0 ~ "Not vaccinated/Don't know if vax",
    vx1718==9 ~ "Error/Missing/Don't know"
  ))

# create indicators for each type of vaccine; denominator
# includes missing/error/don't know
data$shotcomp1718=ifelse(data$vxtype1718comp=="Shot",1,0)
data$novxcomp1718=ifelse(data$vxtype1718comp=="Not vaccinated/Don't know if vax",1,0)
data$errorcomp1718=ifelse(data$vxtype1718comp=="Error/Missing/Don't know",1,0)
table(data$vxtype1718comp[data$vx1718==1],data$dist[data$vx1718==1])

shot.comp.y4.o=as.data.frame(mean_se(Y=data$shotcomp1718[data$dist=="OUSD"],
                                     id=data$matchid[data$dist=="OUSD"]))
novx.comp.y4.o=as.data.frame(mean_se(Y=data$novxcomp1718[data$dist=="OUSD"],
                                     id=data$matchid[data$dist=="OUSD"]))
error.comp.y4.o=as.data.frame(mean_se(Y=data$errorcomp1718[data$dist=="OUSD"],
                                      id=data$matchid[data$dist=="OUSD"]))

shot.comp.y4.w=as.data.frame(mean_se(Y=data$shotcomp1718[data$dist=="WCCUSD"],
                                     id=data$matchid[data$dist=="WCCUSD"]))
novx.comp.y4.w=as.data.frame(mean_se(Y=data$novxcomp1718[data$dist=="WCCUSD"],
                                     id=data$matchid[data$dist=="WCCUSD"]))
error.comp.y4.w=as.data.frame(mean_se(Y=data$errorcomp1718[data$dist=="WCCUSD"],
                                      id=data$matchid[data$dist=="WCCUSD"]))

#-------------------------------------
# vaccine location 
#-------------------------------------
# combine error with missing/dk
data$vxloc1516_18[data$vxloc1516_18=="Error"]="Error/Missing/Don't know"
data$vxloc1516_18[data$vxloc1516_18=="Missing/Don't know"]="Error/Missing/Don't know"
data$vxloc1617_18[data$vxloc1617_18=="Error"]="Error/Missing/Don't know"
data$vxloc1617_18[data$vxloc1617_18=="Missing/Don't know"]="Error/Missing/Don't know"
data$vxloc1718[data$vxloc1718=="Error"]="Error/Missing/Don't know"
data$vxloc1718[data$vxloc1718=="Missing/Don't know"]="Error/Missing/Don't know"

# create indicators for each vaccine location; denominator
# includes missing/error/don't know
data$doc1516_18=ifelse(data$vxloc1516_18=="Doctor/clinic",1,0)
data$doc1617_18=ifelse(data$vxloc1617_18=="Doctor/clinic",1,0)
data$doc1718=ifelse(data$vxloc1718=="Doctor/clinic",1,0)

data$school1516_18=ifelse(data$vxloc1516_18=="School",1,0)
data$school1617_18=ifelse(data$vxloc1617_18=="School",1,0)
data$school1718=ifelse(data$vxloc1718=="School",1,0)

data$other1516_18=ifelse(data$vxloc1516_18=="Other",1,0)
data$other1617_18=ifelse(data$vxloc1617_18=="Other",1,0)
data$other1718=ifelse(data$vxloc1718=="Other",1,0)

data$locerror1516_18=ifelse(data$vxloc1516_18=="Error/Missing/Don't know",1,0)
data$locerror1617_18=ifelse(data$vxloc1617_18=="Error/Missing/Don't know",1,0)
data$locerror1718=ifelse(data$vxloc1718=="Error/Missing/Don't know",1,0)

table(data$vxloc1516_18[data$vx1516_18==1],data$dist[data$vx1516_18==1])
table(data$vxloc1617_18[data$vx1617_18==1],data$dist[data$vx1617_18==1])
table(data$vxloc1718[data$vx1718==1],data$dist[data$vx1718==1])

doc.y2_18.o=as.data.frame(mean_se(Y=data$doc1516_18[data$dist=="OUSD" & data$vx1516_18==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1516_18==1]))
sch.y2_18.o=as.data.frame(mean_se(Y=data$school1516_18[data$dist=="OUSD" & data$vx1516_18==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1516_18==1]))
oth.y2_18.o=as.data.frame(mean_se(Y=data$other1516_18[data$dist=="OUSD" & data$vx1516_18==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1516_18==1]))
errorloc.y2_18.o=as.data.frame(mean_se(Y=data$locerror1516_18[data$dist=="OUSD" & data$vx1516_18==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1516_18==1]))

doc.y3_18.o=as.data.frame(mean_se(Y=data$doc1617_18[data$dist=="OUSD" & data$vx1617_18==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1617_18==1]))
sch.y3_18.o=as.data.frame(mean_se(Y=data$school1617_18[data$dist=="OUSD" & data$vx1617_18==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1617_18==1]))
oth.y3_18.o=as.data.frame(mean_se(Y=data$other1617_18[data$dist=="OUSD" & data$vx1617_18==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1617_18==1]))
errorloc.y3_18.o=as.data.frame(mean_se(Y=data$locerror1617_18[data$dist=="OUSD" & data$vx1617_18==1],
     id=data$matchid[data$dist=="OUSD" & data$vx1617_18==1]))

doc.y4.o=as.data.frame(mean_se(Y=data$doc1718[data$dist=="OUSD" & data$vx1718==1],
    id=data$matchid[data$dist=="OUSD" & data$vx1718==1]))
sch.y4.o=as.data.frame(mean_se(Y=data$school1718[data$dist=="OUSD" & data$vx1718==1],
    id=data$matchid[data$dist=="OUSD" & data$vx1718==1]))
oth.y4.o=as.data.frame(mean_se(Y=data$other1718[data$dist=="OUSD" & data$vx1718==1],
    id=data$matchid[data$dist=="OUSD" & data$vx1718==1]))
errorloc.y4.o=as.data.frame(mean_se(Y=data$locerror1718[data$dist=="OUSD" & data$vx1718==1],
    id=data$matchid[data$dist=="OUSD" & data$vx1718==1]))

doc.y2_18.w=as.data.frame(mean_se(Y=data$doc1516_18[data$dist=="WCCUSD" & data$vx1516_18==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1516_18==1]))
sch.y2_18.w=as.data.frame(mean_se(Y=data$school1516_18[data$dist=="WCCUSD" & data$vx1516_18==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1516_18==1]))
oth.y2_18.w=as.data.frame(mean_se(Y=data$other1516_18[data$dist=="WCCUSD" & data$vx1516_18==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1516_18==1]))
errorloc.y2_18.w=as.data.frame(mean_se(Y=data$locerror1516_18[data$dist=="WCCUSD" & data$vx1516_18==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1516_18==1]))

doc.y3_18.w=as.data.frame(mean_se(Y=data$doc1617_18[data$dist=="WCCUSD" & data$vx1617_18==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1617_18==1]))
sch.y3_18.w=as.data.frame(mean_se(Y=data$school1617_18[data$dist=="WCCUSD" & data$vx1617_18==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1617_18==1]))
oth.y3_18.w=as.data.frame(mean_se(Y=data$other1617_18[data$dist=="WCCUSD" & data$vx1617_18==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1617_18==1]))
errorloc.y3_18.w=as.data.frame(mean_se(Y=data$locerror1617_18[data$dist=="WCCUSD" & data$vx1617_18==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1617_18==1]))

doc.y4.w=as.data.frame(mean_se(Y=data$doc1718[data$dist=="WCCUSD" & data$vx1718==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1718==1]))
sch.y4.w=as.data.frame(mean_se(Y=data$school1718[data$dist=="WCCUSD" & data$vx1718==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1718==1]))
oth.y4.w=as.data.frame(mean_se(Y=data$other1718[data$dist=="WCCUSD" & data$vx1718==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1718==1]))
errorloc.y4.w=as.data.frame(mean_se(Y=data$locerror1718[data$dist=="WCCUSD" & data$vx1718==1],
     id=data$matchid[data$dist=="WCCUSD" & data$vx1718==1]))

#-------------------------------------
# vaccine location, including non vaccinated
#-------------------------------------
data$vxloc1516_18comp=data$vxloc1516_18
data$vxloc1516_18comp[data$vx1516_18!=1]="Not vaccinated/Don't know if vax"
data$vxloc1617_18comp=data$vxloc1617_18
data$vxloc1617_18comp[data$vx1617_18!=1]="Not vaccinated/Don't know if vax"
data$vxloc1718comp=data$vxloc1718
data$vxloc1718comp[data$vx1718!=1]="Not vaccinated/Don't know if vax"

# create indicators for each vaccine location; denominator
# includes missing/error/don't know
data$doccomp1516_18=ifelse(data$vxloc1516_18comp=="Doctor/clinic",1,0)
data$doccomp1617_18=ifelse(data$vxloc1617_18comp=="Doctor/clinic",1,0)
data$doccomp1718=ifelse(data$vxloc1718comp=="Doctor/clinic",1,0)

data$schoolcomp1516_18=ifelse(data$vxloc1516_18comp=="School",1,0)
data$schoolcomp1617_18=ifelse(data$vxloc1617_18comp=="School",1,0)
data$schoolcomp1718=ifelse(data$vxloc1718comp=="School",1,0)

data$othercomp1516_18=ifelse(data$vxloc1516_18comp=="Other",1,0)
data$othercomp1617_18=ifelse(data$vxloc1617_18comp=="Other",1,0)
data$othercomp1718=ifelse(data$vxloc1718comp=="Other",1,0)

data$locerrorcomp1516_18=ifelse(data$vxloc1516_18comp=="Error/Missing/Don't know",1,0)
data$locerrorcomp1617_18=ifelse(data$vxloc1617_18comp=="Error/Missing/Don't know",1,0)
data$locerrorcomp1718=ifelse(data$vxloc1718comp=="Error/Missing/Don't know",1,0)

data$novxcomp1516_18=ifelse(data$vxloc1516_18comp=="Not vaccinated/Don't know if vax",1,0)
data$novxcomp1617_18=ifelse(data$vxloc1617_18comp=="Not vaccinated/Don't know if vax",1,0)
data$novxcomp1718=ifelse(data$vxloc1718comp=="Not vaccinated/Don't know if vax",1,0)

table(data$vxloc1516_18comp,data$dist)
table(data$vxloc1617_18comp,data$dist)
table(data$vxloc1718comp,data$dist)

doc.comp.y2_18.o=as.data.frame(mean_se(Y=data$doccomp1516_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
sch.comp.y2_18.o=as.data.frame(mean_se(Y=data$schoolcomp1516_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
oth.comp.y2_18.o=as.data.frame(mean_se(Y=data$othercomp1516_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
novxloc.comp.y2_18.o=as.data.frame(mean_se(Y=data$novxcomp1516_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
errorloc.comp.y2_18.o=as.data.frame(mean_se(Y=data$locerrorcomp1516_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))

doc.comp.y3_18.o=as.data.frame(mean_se(Y=data$doccomp1617_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
sch.comp.y3_18.o=as.data.frame(mean_se(Y=data$schoolcomp1617_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
oth.comp.y3_18.o=as.data.frame(mean_se(Y=data$othercomp1617_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
novxloc.comp.y3_18.o=as.data.frame(mean_se(Y=data$novxcomp1617_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
errorloc.comp.y3_18.o=as.data.frame(mean_se(Y=data$locerrorcomp1617_18[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))

doc.comp.y4.o=as.data.frame(mean_se(Y=data$doccomp1718[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
sch.comp.y4.o=as.data.frame(mean_se(Y=data$schoolcomp1718[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
oth.comp.y4.o=as.data.frame(mean_se(Y=data$othercomp1718[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
novxloc.comp.y4.o=as.data.frame(mean_se(Y=data$novxcomp1718[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))
errorloc.comp.y4.o=as.data.frame(mean_se(Y=data$locerrorcomp1718[data$dist=="OUSD"],
     id=data$matchid[data$dist=="OUSD"]))

doc.comp.y2_18.w=as.data.frame(mean_se(Y=data$doccomp1516_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
sch.comp.y2_18.w=as.data.frame(mean_se(Y=data$schoolcomp1516_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
oth.comp.y2_18.w=as.data.frame(mean_se(Y=data$othercomp1516_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
novxloc.comp.y2_18.w=as.data.frame(mean_se(Y=data$novxcomp1516_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
errorloc.comp.y2_18.w=as.data.frame(mean_se(Y=data$locerrorcomp1516_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))

doc.comp.y3_18.w=as.data.frame(mean_se(Y=data$doccomp1617_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
sch.comp.y3_18.w=as.data.frame(mean_se(Y=data$schoolcomp1617_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
oth.comp.y3_18.w=as.data.frame(mean_se(Y=data$othercomp1617_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
novxloc.comp.y3_18.w=as.data.frame(mean_se(Y=data$novxcomp1617_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
errorloc.comp.y3_18.w=as.data.frame(mean_se(Y=data$locerrorcomp1617_18[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))

doc.comp.y4.w=as.data.frame(mean_se(Y=data$doccomp1718[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
sch.comp.y4.w=as.data.frame(mean_se(Y=data$schoolcomp1718[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
oth.comp.y4.w=as.data.frame(mean_se(Y=data$othercomp1718[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
novxloc.comp.y4.w=as.data.frame(mean_se(Y=data$novxcomp1718[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))
errorloc.comp.y4.w=as.data.frame(mean_se(Y=data$locerrorcomp1718[data$dist=="WCCUSD"],
     id=data$matchid[data$dist=="WCCUSD"]))

#---------------------------------------
# reasons for not vaccinating
#---------------------------------------
# drop responses for people who did get vaccinated this year
length(data$whynot[data$vx1718==0 & data$dist=="OUSD"])
length(data$whynot[data$vx1718==0 & data$dist=="WCCUSD"])
length(data$whynot[data$vx1718==1 & data$dist=="OUSD"])
length(data$whynot[data$vx1718==1 & data$dist=="WCCUSD"])
length(data$whynot[data$vx1718==9])

data <- data %>%
  mutate(whynot.cost=case_when(
    whynot_2 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_7 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_9 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_4 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_10 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_11 ==1 & (vx1718==0 | vx1718==9)~ 1,
    whynot_12 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    
    whynot_2 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_7 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_9 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_4 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_10 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_11 ==0 & (vx1718==0 | vx1718==9)~ 0,
    whynot_12 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    vx1718==1 ~ NA_real_ 
  )) %>%
  mutate(whynot.trust=case_when(
    whynot_1 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_3 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_5 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_8 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    whynot_6 ==1 & (vx1718==0 | vx1718==9) ~ 1,
    
    whynot_1 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_3 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_5 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_8 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    whynot_6 ==0 & (vx1718==0 | vx1718==9) ~ 0,
    vx1718==1 ~ NA_real_ 
  ))

whynot.cost.res.o=as.data.frame(mean_se(Y=data$whynot.cost[data$dist=="OUSD"],
                                        id=data$matchid[data$dist=="OUSD"]))
whynot.cost.res.w=as.data.frame(mean_se(Y=data$whynot.cost[data$dist=="WCCUSD"],
                                         id=data$matchid[data$dist=="WCCUSD"]))

whynot.trust.res.o=as.data.frame(mean_se(Y=data$whynot.trust[data$dist=="OUSD"],
                                        id=data$matchid[data$dist=="OUSD"]))
whynot.trust.res.w=as.data.frame(mean_se(Y=data$whynot.trust[data$dist=="WCCUSD"],
                                        id=data$matchid[data$dist=="WCCUSD"]))

rm(data, mean_se)
save.image(file=vax_results_2018_path)

