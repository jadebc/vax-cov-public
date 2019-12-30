########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Estimate vaccination coverage from 2017 & 2018 survey
# standardizing by race and education
########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

data = read.csv(data_path_2017)
data.y4 = read.csv(data_path_2018)

load(district_demographics_path)

data$dist=factor(data$dist,levels=c("WCCUSD","OUSD"))
data.y4$dist=factor(data.y4$dist,levels=c("WCCUSD","OUSD"))

# drop Wilson Elementary because it has no matched pair
data=data[data$school!=44,]

data$vx1415yn=ifelse(data$vx1415==1,1,0)
data$vx1516yn=ifelse(data$vx1516==1,1,0)
data$vx1617yn=ifelse(data$vx1617==1,1,0)
data.y4$vx1718yn=ifelse(data.y4$vx1718==1,1,0)

#----------------------------------------
# crude estimates
#----------------------------------------
# vaccine coverage
fit.vx.y1=glm(vx1415yn~dist,data=data)
vcovCL.vx.y1 <- sandwichSE(fm = fit.vx.y1, cluster = data$matchid)
rfit.vx.y1 <- coeftest(fit.vx.y1, vcovCL.vx.y1)
res.vx.y1=format.glm(rfit.vx.y1,family="gaussian")

fit.vx.y2=glm(vx1516yn~dist,data=data)
vcovCL.vx.y2 <- sandwichSE(fm = fit.vx.y2, cluster = data$matchid)
rfit.vx.y2 <- coeftest(fit.vx.y2, vcovCL.vx.y2)
res.vx.y2=format.glm(rfit.vx.y2,family="gaussian")

fit.vx.y3=glm(vx1617yn~dist,data=data)
vcovCL.vx.y3 <- sandwichSE(fm = fit.vx.y3, cluster = data$matchid)
rfit.vx.y3 <- coeftest(fit.vx.y3, vcovCL.vx.y3)
res.vx.y3=format.glm(rfit.vx.y3,family="gaussian")

fit.vx.y4=glm(vx1718yn~dist,data=data.y4)
vcovCL.vx.y4 <- sandwichSE(fm = fit.vx.y4, cluster = data.y4$matchid)
rfit.vx.y4 <- coeftest(fit.vx.y4, vcovCL.vx.y4)
res.vx.y4=format.glm(rfit.vx.y4,family="gaussian")

# difference in % vaccinated at doctor in year 3
data$vxloc1617comp=as.character(data$vxloc1617)
data$vxloc1617comp[data$vx1617!=1]="Not vaccinated/Don't know if vax"
data$doccomp1617=ifelse(data$vxloc1617comp=="Doctor/clinic",1,0)

fit.vx.doc.y3=glm(doccomp1617~dist,data=data)
vcovCL.vx.doc.y3 <- sandwichSE(fm = fit.vx.doc.y3, cluster = data$matchid)
rfit.vx.doc.y3 <- coeftest(fit.vx.doc.y3, vcovCL.vx.doc.y3)
res.vx.doc.y3=format.glm(rfit.vx.doc.y3,family="gaussian")

# adjusted
fit.vx.y1.adj=glm(vx1617yn~dist + race + edu,data=data)
vcovCL.vx.y1.adj <- sandwichSE(fm = fit.vx.y1.adj, cluster = data$matchid)
rfit.vx.y1.adj <- coeftest(fit.vx.y1, vcovCL.vx.y1)
res.vx.y1.adj=format.glm(rfit.vx.y1.adj,family="gaussian")

fit.vx.y2.adj=glm(vx1617yn~dist + race + edu,data=data)
vcovCL.vx.y2.adj <- sandwichSE(fm = fit.vx.y2.adj, cluster = data$matchid)
rfit.vx.y2.adj <- coeftest(fit.vx.y2, vcovCL.vx.y2)
res.vx.y2.adj=format.glm(rfit.vx.y2.adj,family="gaussian")

fit.vx.y3.adj=glm(vx1617yn~dist + race + edu,data=data)
vcovCL.vx.y3.adj <- sandwichSE(fm = fit.vx.y3.adj, cluster = data$matchid)
rfit.vx.y3.adj <- coeftest(fit.vx.y3, vcovCL.vx.y3)
res.vx.y3.adj=format.glm(rfit.vx.y3.adj,family="gaussian")

fit.vx.y4.adj=glm(vx1718yn~dist + as.factor(race) + as.factor(edu),data=data.y4)
vcovCL.vx.y4.adj <- sandwichSE(fm = fit.vx.y4.adj, cluster = data.y4$matchid)
rfit.vx.y4.adj <- coeftest(fit.vx.y4, vcovCL.vx.y4)
res.vx.y4.adj=format.glm(rfit.vx.y4.adj,family="gaussian")

#----------------------------------------
# standardize by education
#----------------------------------------
#--------------
# 2017 survey
# drop rows with no edu information or errors
data.edu=data[!data$edu %in% "Error",]
data.edu=data.edu[!is.na(data.edu$edu),]
data.edu$edu=droplevels(data.edu$edu)

# calculate weights  - whole district
# weights defined as P(Z=z in reference pop) * P(z=z in study pop)

colnames(dist.edu)=c("edu","OUSD","WCCUSD")
pz.edu.dist=melt(dist.edu)
colnames(pz.edu.dist)=c("edu","dist","pz.dist")
pz.edu.dist$pz.dist=pz.edu.dist$pz.dist/100
pz.edu.dist$edu=as.character(pz.edu.dist$edu)
pz.edu.dist$edu[pz.edu.dist$edu=="High school graduate"]="High school"
pz.edu.dist$edu[pz.edu.dist$edu=="College graduate"]="Associate/College"
pz.edu.dist$edu[pz.edu.dist$edu=="Graduate school"]="Postgrad"

# calculate weights  - whole schools in sample
colnames(dist.edu.s)=c("edu","OUSD","WCCUSD")
pz.edu.dist.s=melt(dist.edu.s)
colnames(pz.edu.dist.s)=c("edu","dist","pz.dist.s")
pz.edu.dist.s$pz.dist.s=pz.edu.dist.s$pz.dist.s/100
pz.edu.dist.s$edu=as.character(pz.edu.dist.s$edu)
pz.edu.dist.s$edu[pz.edu.dist.s$edu=="High school graduate"]="High school"
pz.edu.dist.s$edu[pz.edu.dist.s$edu=="College graduate"]="Associate/College"
pz.edu.dist.s$edu[pz.edu.dist.s$edu=="Graduate school"]="Postgrad"

# P(Z=z) in the survey sample
pz.edu.samp=as.data.frame(table(data.edu$edu,data.edu$dist))
colnames(pz.edu.samp)=c("edu","dist","pz")

# merge weights
wt.edu=merge(pz.edu.samp,pz.edu.dist,by=c("dist","edu"))
wt.edu=merge(wt.edu,pz.edu.dist.s,by=c("dist","edu"))
wt.edu$wt.dist=(1/wt.edu$pz)*wt.edu$pz.dist
wt.edu$wt.dist.s=(1/wt.edu$pz)*wt.edu$pz.dist.s

data.edu=merge(data.edu,wt.edu[,c("edu","dist","wt.dist","wt.dist.s")],
      by=c("edu","dist"))

#--------------
# 2018 survey
# drop rows with no edu information or errors
data.y4.edu=data.y4[!data.y4$edu %in% "Error",]
data.y4.edu=data.y4.edu[!is.na(data.y4.edu$edu),]
data.y4.edu$edu=droplevels(data.y4.edu$edu)

# calculate weights  - whole district
# weights defined as P(Z=z in reference pop) * P(z=z in study pop)

colnames(dist.edu)=c("edu","OUSD","WCCUSD")
pz.edu.dist.y4=melt(dist.edu)
colnames(pz.edu.dist.y4)=c("edu","dist","pz.dist")
pz.edu.dist.y4$pz.dist=pz.edu.dist.y4$pz.dist/100
pz.edu.dist.y4$edu=as.character(pz.edu.dist.y4$edu)
pz.edu.dist.y4$edu[pz.edu.dist.y4$edu=="High school graduate"]="High school"
pz.edu.dist.y4$edu[pz.edu.dist.y4$edu=="College graduate"]="Associate/College"
pz.edu.dist.y4$edu[pz.edu.dist.y4$edu=="Graduate school"]="Postgrad"

# calculate weights  - whole schools in sample
colnames(dist.edu.s)=c("edu","OUSD","WCCUSD")
pz.edu.dist.y4.s=melt(dist.edu.s)
colnames(pz.edu.dist.y4.s)=c("edu","dist","pz.dist.s")
pz.edu.dist.y4.s$pz.dist.s=pz.edu.dist.y4.s$pz.dist.s/100
pz.edu.dist.y4.s$edu=as.character(pz.edu.dist.y4.s$edu)
pz.edu.dist.y4.s$edu[pz.edu.dist.y4.s$edu=="High school graduate"]="High school"
pz.edu.dist.y4.s$edu[pz.edu.dist.y4.s$edu=="College graduate"]="Associate/College"
pz.edu.dist.y4.s$edu[pz.edu.dist.y4.s$edu=="Graduate school"]="Postgrad"

# P(Z=z) in the survey sample
pz.edu.samp.y4=as.data.frame(table(data.y4.edu$edu,data.y4.edu$dist))
colnames(pz.edu.samp.y4)=c("edu","dist","pz")

# merge weights
wt.edu.y4=merge(pz.edu.samp.y4,pz.edu.dist.y4,by=c("dist","edu"))
wt.edu.y4=merge(wt.edu.y4,pz.edu.dist.y4.s,by=c("dist","edu"))
wt.edu.y4$wt.dist=(1/wt.edu.y4$pz)*wt.edu.y4$pz.dist
wt.edu.y4$wt.dist.s=(1/wt.edu.y4$pz)*wt.edu.y4$pz.dist.s

data.y4.edu=merge(data.y4.edu,wt.edu.y4[,c("edu","dist","wt.dist","wt.dist.s")],
                  by=c("edu","dist"))

# weighted regression - whole district
fit.vx.y1.edu.dist=glm(vx1415yn~dist,data=data.edu,weights=wt.dist)
vcovCL.vx.y1.edu.dist <- sandwichSE(fm = fit.vx.y1.edu.dist, cluster = data.edu$matchid)
rfit.vx.y1.edu.dist <- coeftest(fit.vx.y1.edu.dist, vcovCL.vx.y1.edu.dist)
res.vx.y1.edu.dist=format.glm(rfit.vx.y1.edu.dist,family="gaussian")
pred.vx.y1.edu.dist=getpred(fit.vx.y1.edu.dist,vcovCL.vx.y1.edu.dist)

fit.vx.y2.edu.dist=glm(vx1516yn~dist,data=data.edu,weights=wt.dist)
vcovCL.vx.y2.edu.dist <- sandwichSE(fm = fit.vx.y2.edu.dist, cluster = data.edu$matchid)
rfit.vx.y2.edu.dist <- coeftest(fit.vx.y2.edu.dist, vcovCL.vx.y2.edu.dist)
res.vx.y2.edu.dist=format.glm(rfit.vx.y2.edu.dist,family="gaussian")
pred.vx.y2.edu.dist=getpred(fit.vx.y2.edu.dist,vcovCL.vx.y2.edu.dist)

fit.vx.y3.edu.dist=glm(vx1617yn~dist,data=data.edu,weights=wt.dist)
vcovCL.vx.y3.edu.dist <- sandwichSE(fm = fit.vx.y3.edu.dist, cluster = data.edu$matchid)
rfit.vx.y3.edu.dist <- coeftest(fit.vx.y3.edu.dist, vcovCL.vx.y3.edu.dist)
res.vx.y3.edu.dist=format.glm(rfit.vx.y3.edu.dist,family="gaussian")
pred.vx.y3.edu.dist=getpred(fit.vx.y3.edu.dist,vcovCL.vx.y3.edu.dist)

fit.vx.y4.edu.dist=glm(vx1718yn~dist,data=data.y4.edu,weights=wt.dist)
vcovCL.vx.y4.edu.dist <- sandwichSE(fm = fit.vx.y4.edu.dist, cluster = data.y4.edu$matchid)
rfit.vx.y4.edu.dist <- coeftest(fit.vx.y4.edu.dist, vcovCL.vx.y4.edu.dist)
res.vx.y4.edu.dist=format.glm(rfit.vx.y4.edu.dist,family="gaussian")
pred.vx.y4.edu.dist=getpred(fit.vx.y4.edu.dist,vcovCL.vx.y4.edu.dist)

# weighted regression - whole schools in sample
fit.vx.y1.edu.dist.s=glm(vx1415yn~dist,data=data.edu,weights=wt.dist.s)
vcovCL.vx.y1.edu.dist.s <- sandwichSE(fm = fit.vx.y1.edu.dist.s, cluster = data.edu$matchid)
rfit.vx.y1.edu.dist.s <- coeftest(fit.vx.y1.edu.dist.s, vcovCL.vx.y1.edu.dist.s)
res.vx.y1.edu.dist.s=format.glm(rfit.vx.y1.edu.dist.s,family="gaussian")
pred.vx.y1.edu.dist.s=getpred(fit.vx.y1.edu.dist.s,vcovCL.vx.y1.edu.dist.s)

fit.vx.y2.edu.dist.s=glm(vx1516yn~dist,data=data.edu,weights=wt.dist.s)
vcovCL.vx.y2.edu.dist.s <- sandwichSE(fm = fit.vx.y2.edu.dist.s, cluster = data.edu$matchid)
rfit.vx.y2.edu.dist.s <- coeftest(fit.vx.y2.edu.dist.s, vcovCL.vx.y2.edu.dist.s)
res.vx.y2.edu.dist.s=format.glm(rfit.vx.y2.edu.dist.s,family="gaussian")
pred.vx.y2.edu.dist.s=getpred(fit.vx.y2.edu.dist.s,vcovCL.vx.y2.edu.dist.s)

fit.vx.y3.edu.dist.s=glm(vx1617yn~dist,data=data.edu,weights=wt.dist.s)
vcovCL.vx.y3.edu.dist.s <- sandwichSE(fm = fit.vx.y3.edu.dist.s, cluster = data.edu$matchid)
rfit.vx.y3.edu.dist.s <- coeftest(fit.vx.y3.edu.dist.s, vcovCL.vx.y3.edu.dist.s)
res.vx.y3.edu.dist.s=format.glm(rfit.vx.y3.edu.dist.s,family="gaussian")
pred.vx.y3.edu.dist.s=getpred(fit.vx.y3.edu.dist.s,vcovCL.vx.y3.edu.dist.s)

fit.vx.y4.edu.dist.s=glm(vx1718yn~dist,data=data.y4.edu,weights=wt.dist.s)
vcovCL.vx.y4.edu.dist.s <- sandwichSE(fm = fit.vx.y4.edu.dist.s, cluster = data.y4.edu$matchid)
rfit.vx.y4.edu.dist.s <- coeftest(fit.vx.y4.edu.dist.s, vcovCL.vx.y4.edu.dist.s)
res.vx.y4.edu.dist.s=format.glm(rfit.vx.y4.edu.dist.s,family="gaussian")
pred.vx.y4.edu.dist.s=getpred(fit.vx.y4.edu.dist.s,vcovCL.vx.y4.edu.dist.s)

#----------------------------------------
# standardize by race
#----------------------------------------
#--------------
# 2017 survey
data$race=as.character(data$race)
data$race[data$race=="Multi"]="Multiple"
data$race[data$race=="Black"]="African American"
data$race[data$race=="Pacific islander"]="Pacific Islander"
data$race[data$race=="Missing"]="Not reported"

# calculate weights  - whole district
# weights defined as P(Z=z in reference pop) * P(z=z in study pop)

colnames(dist.race)=c("race","OUSD","WCCUSD")
pz.race.dist=melt(dist.race)
colnames(pz.race.dist)=c("race","dist","pz.dist")
pz.race.dist$pz.dist=pz.race.dist$pz.dist/100
pz.race.dist$race=as.character(pz.race.dist$race)

# calculate weights  - whole schools in sample
colnames(dist.race.s)=c("race","OUSD","WCCUSD")
pz.race.dist.s=melt(dist.race.s)
colnames(pz.race.dist.s)=c("race","dist","pz.dist.s")
pz.race.dist.s$pz.dist.s=pz.race.dist.s$pz.dist.s/100
pz.race.dist.s$race=as.character(pz.race.dist.s$race)

# P(Z=z) in the survey sample
pz.race.samp=as.data.frame(table(data$race,data$dist))
colnames(pz.race.samp)=c("race","dist","pz")

# merge weights
wt.race=merge(pz.race.samp,pz.race.dist,by=c("dist","race"))
wt.race=merge(wt.race,pz.race.dist.s,by=c("dist","race"))
wt.race$wt.dist=(1/wt.race$pz)*wt.race$pz.dist
wt.race$wt.dist.s=(1/wt.race$pz)*wt.race$pz.dist.s

data.race=merge(data,wt.race[,c("race","dist","wt.dist","wt.dist.s")],
               by=c("race","dist"))

#--------------
# 2018 survey
data.y4$race=as.character(data.y4$race)
data.y4$race[data.y4$race=="Multi"]="Multiple"
data.y4$race[data.y4$race=="Black"]="African American"
data.y4$race[data.y4$race=="Pacific islander"]="Pacific Islander"
data.y4$race[data.y4$race=="Missing"]="Not reported"

# calculate weights  - whole district
# weights defined as P(Z=z in reference pop) * P(z=z in study pop)

colnames(dist.race)=c("race","OUSD","WCCUSD")
pz.race.dist.y4=melt(dist.race)
colnames(pz.race.dist.y4)=c("race","dist","pz.dist")
pz.race.dist.y4$pz.dist=pz.race.dist.y4$pz.dist/100
pz.race.dist.y4$race=as.character(pz.race.dist.y4$race)

# calculate weights  - whole schools in sample
colnames(dist.race.s)=c("race","OUSD","WCCUSD")
pz.race.dist.y4.s=melt(dist.race.s)
colnames(pz.race.dist.y4.s)=c("race","dist","pz.dist.s")
pz.race.dist.y4.s$pz.dist.s=pz.race.dist.y4.s$pz.dist.s/100
pz.race.dist.y4.s$race=as.character(pz.race.dist.y4.s$race)

# P(Z=z) in the survey sample
pz.race.samp.y4=as.data.frame(table(data.y4$race,data.y4$dist))
colnames(pz.race.samp.y4)=c("race","dist","pz")

# merge weights
wr.race.y4=merge(pz.race.samp.y4,pz.race.dist.y4,by=c("dist","race"))
wr.race.y4=merge(wr.race.y4,pz.race.dist.y4.s,by=c("dist","race"))
wr.race.y4$wt.dist=(1/wr.race.y4$pz)*wr.race.y4$pz.dist
wr.race.y4$wt.dist.s=(1/wr.race.y4$pz)*wr.race.y4$pz.dist.s

data.y4.race=merge(data.y4,wr.race.y4[,c("race","dist","wt.dist","wt.dist.s")],
                   by=c("race","dist"))

# weighted regression - whole district
fit.vx.y1.race.dist=glm(vx1415yn~dist,data=data.race,weights=wt.dist)
vcovCL.vx.y1.race.dist <- sandwichSE(fm = fit.vx.y1.race.dist, cluster = data.race$matchid)
rfit.vx.y1.race.dist <- coeftest(fit.vx.y1.race.dist, vcovCL.vx.y1.race.dist)
res.vx.y1.race.dist=format.glm(rfit.vx.y1.race.dist,family="gaussian")
pred.vx.y1.race.dist=getpred(fit.vx.y1.race.dist,vcovCL.vx.y1.race.dist)

fit.vx.y2.race.dist=glm(vx1516yn~dist,data=data.race,weights=wt.dist)
vcovCL.vx.y2.race.dist <- sandwichSE(fm = fit.vx.y2.race.dist, cluster = data.race$matchid)
rfit.vx.y2.race.dist <- coeftest(fit.vx.y2.race.dist, vcovCL.vx.y2.race.dist)
res.vx.y2.race.dist=format.glm(rfit.vx.y2.race.dist,family="gaussian")
pred.vx.y2.race.dist=getpred(fit.vx.y2.race.dist,vcovCL.vx.y2.race.dist)

fit.vx.y3.race.dist=glm(vx1617yn~dist,data=data.race,weights=wt.dist)
vcovCL.vx.y3.race.dist <- sandwichSE(fm = fit.vx.y3.race.dist, cluster = data.race$matchid)
rfit.vx.y3.race.dist <- coeftest(fit.vx.y3.race.dist, vcovCL.vx.y3.race.dist)
res.vx.y3.race.dist=format.glm(rfit.vx.y3.race.dist,family="gaussian")
pred.vx.y3.race.dist=getpred(fit.vx.y3.race.dist,vcovCL.vx.y3.race.dist)

fit.vx.y4.race.dist=glm(vx1718yn~dist,data=data.y4.race,weights=wt.dist)
vcovCL.vx.y4.race.dist <- sandwichSE(fm = fit.vx.y4.race.dist, cluster = data.y4.race$matchid)
rfit.vx.y4.race.dist <- coeftest(fit.vx.y4.race.dist, vcovCL.vx.y4.race.dist)
res.vx.y4.race.dist=format.glm(rfit.vx.y4.race.dist,family="gaussian")
pred.vx.y4.race.dist=getpred(fit.vx.y4.race.dist,vcovCL.vx.y4.race.dist)

# weighted regression - whole schools in sample
fit.vx.y1.race.dist.s=glm(vx1415yn~dist,data=data.race,weights=wt.dist.s)
vcovCL.vx.y1.race.dist.s <- sandwichSE(fm = fit.vx.y1.race.dist.s, cluster = data.race$matchid)
rfit.vx.y1.race.dist.s <- coeftest(fit.vx.y1.race.dist.s, vcovCL.vx.y1.race.dist.s)
res.vx.y1.race.dist.s=format.glm(rfit.vx.y1.race.dist.s,family="gaussian")
pred.vx.y1.race.dist.s=getpred(fit.vx.y1.race.dist.s,vcovCL.vx.y1.race.dist.s)

fit.vx.y2.race.dist.s=glm(vx1516yn~dist,data=data.race,weights=wt.dist.s)
vcovCL.vx.y2.race.dist.s <- sandwichSE(fm = fit.vx.y2.race.dist.s, cluster = data.race$matchid)
rfit.vx.y2.race.dist.s <- coeftest(fit.vx.y2.race.dist.s, vcovCL.vx.y2.race.dist.s)
res.vx.y2.race.dist.s=format.glm(rfit.vx.y2.race.dist.s,family="gaussian")
pred.vx.y2.race.dist.s=getpred(fit.vx.y2.race.dist.s,vcovCL.vx.y2.race.dist.s)

fit.vx.y3.race.dist.s=glm(vx1617yn~dist,data=data.race,weights=wt.dist.s)
vcovCL.vx.y3.race.dist.s <- sandwichSE(fm = fit.vx.y3.race.dist.s, cluster = data.race$matchid)
rfit.vx.y3.race.dist.s <- coeftest(fit.vx.y3.race.dist.s, vcovCL.vx.y3.race.dist.s)
res.vx.y3.race.dist.s=format.glm(rfit.vx.y3.race.dist.s,family="gaussian")
pred.vx.y3.race.dist.s=getpred(fit.vx.y3.race.dist.s,vcovCL.vx.y3.race.dist.s)

fit.vx.y4.race.dist.s=glm(vx1718yn~dist,data=data.y4.race,weights=wt.dist.s)
vcovCL.vx.y4.race.dist.s <- sandwichSE(fm = fit.vx.y4.race.dist.s, cluster = data.y4.race$matchid)
rfit.vx.y4.race.dist.s <- coeftest(fit.vx.y4.race.dist.s, vcovCL.vx.y4.race.dist.s)
res.vx.y4.race.dist.s=format.glm(rfit.vx.y4.race.dist.s,family="gaussian")
pred.vx.y4.race.dist.s=getpred(fit.vx.y4.race.dist.s,vcovCL.vx.y4.race.dist.s)

save(res.vx.y1,res.vx.y2,res.vx.y3,res.vx.y4,
     res.vx.y1.edu.dist,res.vx.y2.edu.dist,res.vx.y3.edu.dist,res.vx.y4.edu.dist,
     res.vx.y1.edu.dist.s,res.vx.y2.edu.dist.s,res.vx.y3.edu.dist.s,res.vx.y4.edu.dist.s,
     res.vx.y1.race.dist,res.vx.y2.race.dist,res.vx.y3.race.dist,res.vx.y4.race.dist,
     res.vx.y1.race.dist.s,res.vx.y2.race.dist.s,res.vx.y3.race.dist.s,res.vx.y4.race.dist.s,
     pred.vx.y1.edu.dist,pred.vx.y2.edu.dist,pred.vx.y3.edu.dist,pred.vx.y4.edu.dist,
     pred.vx.y1.edu.dist.s,pred.vx.y2.edu.dist.s,pred.vx.y3.edu.dist.s,pred.vx.y4.edu.dist.s,
     pred.vx.y1.race.dist,pred.vx.y2.race.dist,pred.vx.y3.race.dist,pred.vx.y4.race.dist,
     pred.vx.y1.race.dist.s,pred.vx.y2.race.dist.s,pred.vx.y3.race.dist.s,pred.vx.y4.race.dist.s,
     file=vax_standardized_results_path)


