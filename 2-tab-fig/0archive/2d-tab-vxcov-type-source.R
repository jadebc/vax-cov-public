############################################
# Vaccine coverage survey conducted in March 2017
# Table of vaccine source and location
############################################

rm(list=ls())
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_results.RData")
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_std_results.RData")
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/0-base-functions-tabfig.R")

tab.dir="~/Dropbox/Flu/StFData/Vax cov/Tables/"

# Format results for vaccine type
y1.o.type=rbind(shot.comp.y1.o,spray.comp.y1.o,novx.comp.y1.o,error.comp.y1.o)
y2.o.type=rbind(shot.comp.y2.o,spray.comp.y2.o,novx.comp.y2.o,error.comp.y2.o)
y3.o.type=rbind(shot.comp.y3.o,spray.comp.y3.o,novx.comp.y3.o,error.comp.y3.o)

y1.w.type=rbind(shot.comp.y1.w,spray.comp.y1.w,error.comp.y1.w,novx.comp.y1.w)
y2.w.type=rbind(shot.comp.y2.w,spray.comp.y2.w,error.comp.y2.w,novx.comp.y2.w)
y3.w.type=rbind(shot.comp.y3.w,spray.comp.y3.w,error.comp.y3.w,novx.comp.y3.w)

y1.o.type.f=apply(y1.o.type[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)
y2.o.type.f=apply(y2.o.type[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)
y3.o.type.f=apply(y3.o.type[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)

y1.w.type.f=apply(y1.w.type[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)
y2.w.type.f=apply(y2.w.type[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)
y3.w.type.f=apply(y3.w.type[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)

# Format results for vaccine source
y1.o.loc=rbind(doc.comp.y1.o,sch.comp.y1.o,oth.comp.y1.o,novxloc.comp.y1.o,errorloc.comp.y1.o)
y2.o.loc=rbind(doc.comp.y2.o,sch.comp.y2.o,oth.comp.y2.o,novxloc.comp.y2.o,errorloc.comp.y2.o)
y3.o.loc=rbind(doc.comp.y3.o,sch.comp.y3.o,oth.comp.y3.o,novxloc.comp.y3.o,errorloc.comp.y3.o)

y1.w.loc=rbind(doc.comp.y1.w,sch.comp.y1.w,oth.comp.y1.w,novxloc.comp.y1.w,errorloc.comp.y1.w)
y2.w.loc=rbind(doc.comp.y2.w,sch.comp.y2.w,oth.comp.y2.w,novxloc.comp.y2.w,errorloc.comp.y2.w)
y3.w.loc=rbind(doc.comp.y3.w,sch.comp.y3.w,oth.comp.y3.w,novxloc.comp.y3.w,errorloc.comp.y3.w)

y1.o.loc.f=apply(y1.o.loc[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)
y2.o.loc.f=apply(y2.o.loc[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)
y3.o.loc.f=apply(y3.o.loc[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)

y1.w.loc.f=apply(y1.w.loc[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)
y2.w.loc.f=apply(y2.w.loc[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)
y3.w.loc.f=apply(y3.w.loc[,c("Mean","Lower 95%CI","Upper 95%CI")],1,pt.est.ci.f,0,100)

# Format table
tab.type.loc=as.data.frame(rbind(cbind(shot.comp.y1.o$N,y1.o.type.f,y2.o.type.f,y3.o.type.f,
      shot.comp.y1.w$N,y1.w.type.f,y2.w.type.f,y3.w.type.f),
	cbind(doc.comp.y1.o$N,y1.o.loc.f,y2.o.loc.f,y3.o.loc.f,
	      doc.comp.y1.w$N,y1.w.loc.f,y2.w.loc.f,y3.w.loc.f)))
rownames(tab.type.loc)=NULL

tab.type.loc$label=c("~~~Shot","~~~Spray","~~~Not vaccinated$^\\text{b}$","~~~Error",
                     "~~~Doctor/health clinic",
                     "~~~School","~~~Other","~~~Not vaccinated$^\\text{b}$","~~~Error")

tab.type.loc=tab.type.loc[,c(9,1:8)]

save(tab.type.loc,file=paste0(tab.dir,"tab-type-loc.RData"))
