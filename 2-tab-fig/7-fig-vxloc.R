########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Create figures showing vaccination source by year
########################################

rm(list=ls())
# define directories, load libraries
source(here::here("0-config.R"))

load(vax_results_2017_path)
load(vax_results_2018_path)

doc.comp.y1.o$loc="Doctor/health clinic"
sch.comp.y1.o$loc="School"
oth.comp.y1.o$loc="Other"
novxloc.comp.y1.o$loc="Not vaccinated"
errorloc.comp.y1.o$loc="Error/missing/don't know"

doc.comp.y2.o$loc="Doctor/health clinic"
sch.comp.y2.o$loc="School"
oth.comp.y2.o$loc="Other"
novxloc.comp.y2.o$loc="Not vaccinated"
errorloc.comp.y2.o$loc="Error/missing/don't know"

doc.comp.y3.o$loc="Doctor/health clinic"
sch.comp.y3.o$loc="School"
oth.comp.y3.o$loc="Other"
novxloc.comp.y3.o$loc="Not vaccinated"
errorloc.comp.y3.o$loc="Error/missing/don't know"

doc.comp.y4.o$loc="Doctor/health clinic"
sch.comp.y4.o$loc="School"
oth.comp.y4.o$loc="Other"
novxloc.comp.y4.o$loc="Not vaccinated"
errorloc.comp.y4.o$loc="Error/missing/don't know"

doc.comp.y1.w$loc="Doctor/health clinic"
sch.comp.y1.w$loc="School"
oth.comp.y1.w$loc="Other"
novxloc.comp.y1.w$loc="Not vaccinated"
errorloc.comp.y1.w$loc="Error/missing/don't know"

doc.comp.y2.w$loc="Doctor/health clinic"
sch.comp.y2.w$loc="School"
oth.comp.y2.w$loc="Other"
novxloc.comp.y2.w$loc="Not vaccinated"
errorloc.comp.y2.w$loc="Error/missing/don't know"

doc.comp.y3.w$loc="Doctor/health clinic"
sch.comp.y3.w$loc="School"
oth.comp.y3.w$loc="Other"
novxloc.comp.y3.w$loc="Not vaccinated"
errorloc.comp.y3.w$loc="Error/missing/don't know"

doc.comp.y4.w$loc="Doctor/health clinic"
sch.comp.y4.w$loc="School"
oth.comp.y4.w$loc="Other"
novxloc.comp.y4.w$loc="Not vaccinated"
errorloc.comp.y4.w$loc="Error/missing/don't know"

doc.comp.y1.o$dist="OUSD"
sch.comp.y1.o$dist="OUSD"
oth.comp.y1.o$dist="OUSD"
novxloc.comp.y1.o$dist="OUSD"
errorloc.comp.y1.o$dist="OUSD"

doc.comp.y2.o$dist="OUSD"
sch.comp.y2.o$dist="OUSD"
oth.comp.y2.o$dist="OUSD"
novxloc.comp.y2.o$dist="OUSD"
errorloc.comp.y2.o$dist="OUSD"

doc.comp.y3.o$dist="OUSD"
sch.comp.y3.o$dist="OUSD"
oth.comp.y3.o$dist="OUSD"
novxloc.comp.y3.o$dist="OUSD"
errorloc.comp.y3.o$dist="OUSD"

doc.comp.y4.o$dist="OUSD"
sch.comp.y4.o$dist="OUSD"
oth.comp.y4.o$dist="OUSD"
novxloc.comp.y4.o$dist="OUSD"
errorloc.comp.y4.o$dist="OUSD"

doc.comp.y1.w$dist="WCCUSD"
sch.comp.y1.w$dist="WCCUSD"
oth.comp.y1.w$dist="WCCUSD"
novxloc.comp.y1.w$dist="WCCUSD"
errorloc.comp.y1.w$dist="WCCUSD"

doc.comp.y2.w$dist="WCCUSD"
sch.comp.y2.w$dist="WCCUSD"
oth.comp.y2.w$dist="WCCUSD"
novxloc.comp.y2.w$dist="WCCUSD"
errorloc.comp.y2.w$dist="WCCUSD"

doc.comp.y3.w$dist="WCCUSD"
sch.comp.y3.w$dist="WCCUSD"
oth.comp.y3.w$dist="WCCUSD"
novxloc.comp.y3.w$dist="WCCUSD"
errorloc.comp.y3.w$dist="WCCUSD"

doc.comp.y4.w$dist="WCCUSD"
sch.comp.y4.w$dist="WCCUSD"
oth.comp.y4.w$dist="WCCUSD"
novxloc.comp.y4.w$dist="WCCUSD"
errorloc.comp.y4.w$dist="WCCUSD"

doc.comp.y1.o$yr="2014-15"
sch.comp.y1.o$yr="2014-15"
oth.comp.y1.o$yr="2014-15"
novxloc.comp.y1.o$yr="2014-15"
errorloc.comp.y1.o$yr="2014-15"

doc.comp.y2.o$yr="2015-16"
sch.comp.y2.o$yr="2015-16"
oth.comp.y2.o$yr="2015-16"
novxloc.comp.y2.o$yr="2015-16"
errorloc.comp.y2.o$yr="2015-16"

doc.comp.y3.o$yr="2016-17"
sch.comp.y3.o$yr="2016-17"
oth.comp.y3.o$yr="2016-17"
novxloc.comp.y3.o$yr="2016-17"
errorloc.comp.y3.o$yr="2016-17"

doc.comp.y4.o$yr="2017-18"
sch.comp.y4.o$yr="2017-18"
oth.comp.y4.o$yr="2017-18"
novxloc.comp.y4.o$yr="2017-18"
errorloc.comp.y4.o$yr="2017-18"

doc.comp.y1.w$yr="2014-15"
sch.comp.y1.w$yr="2014-15"
oth.comp.y1.w$yr="2014-15"
novxloc.comp.y1.w$yr="2014-15"
errorloc.comp.y1.w$yr="2014-15"

doc.comp.y2.w$yr="2015-16"
sch.comp.y2.w$yr="2015-16"
oth.comp.y2.w$yr="2015-16"
novxloc.comp.y2.w$yr="2015-16"
errorloc.comp.y2.w$yr="2015-16"

doc.comp.y3.w$yr="2016-17"
sch.comp.y3.w$yr="2016-17"
oth.comp.y3.w$yr="2016-17"
novxloc.comp.y3.w$yr="2016-17"
errorloc.comp.y3.w$yr="2016-17"

doc.comp.y4.w$yr="2017-18"
sch.comp.y4.w$yr="2017-18"
oth.comp.y4.w$yr="2017-18"
novxloc.comp.y4.w$yr="2017-18"
errorloc.comp.y4.w$yr="2017-18"

vxloc=rbind(doc.comp.y1.o,sch.comp.y1.o,oth.comp.y1.o,errorloc.comp.y1.o,
            doc.comp.y2.o,sch.comp.y2.o,oth.comp.y2.o,errorloc.comp.y2.o,
            doc.comp.y3.o,sch.comp.y3.o,oth.comp.y3.o,errorloc.comp.y3.o,
            doc.comp.y4.o,sch.comp.y4.o,oth.comp.y4.o,errorloc.comp.y4.o,
            doc.comp.y1.w,sch.comp.y1.w,oth.comp.y1.w,errorloc.comp.y1.w,
            doc.comp.y2.w,sch.comp.y2.w,oth.comp.y2.w,errorloc.comp.y2.w,
            doc.comp.y3.w,sch.comp.y3.w,oth.comp.y3.w,errorloc.comp.y3.w,
            doc.comp.y4.w,sch.comp.y4.w,oth.comp.y4.w,errorloc.comp.y4.w)

vxloc$yr=as.factor(vxloc$yr)
vxloc$Mean=vxloc$Mean*100
vxloc$Mean.f=sprintf("%0.0f",vxloc$Mean)

colnames(vxloc)[5:6]=c("lower","upper")
vxloc$lower=vxloc$lower*100
vxloc$upper=vxloc$upper*100

vxloc$loc=factor(vxloc$loc,levels=c("Doctor/health clinic",
    "School","Other","Not vaccinated","Error/missing/don't know"))
levels(vxloc$loc)[5]="Error/Missing/\nDon't know"

vxloc$Mean.f[vxloc$loc=="Other"]=""
vxloc$Mean.f[vxloc$loc=="School" & vxloc$dist=="WCCUSD"]=""
vxloc$Mean.f[vxloc$loc=="Error/Missing/\nDon't know"]=""

vxloc$printmean=c(42,11,0,0,
                  42,14,0,15,
                  42,14,0,15,
                  42,11,0,15,
                  42,11,0,15,
                  42,11,0,15,
                  42,11,0,15,
                  42,11,0,15)

vxloc$dist.new = ifelse(vxloc$dist=="OUSD", "Intervention", "Comparison")

pdf(file=paste0(plot_path,"fig-vxloc.pdf"),width=9,height=4)
ggplot(vxloc,aes(y=Mean,x=dist.new,fill=loc))+geom_bar(stat="identity", width=0.75)+
  scale_fill_manual("",values=c("#2185c5","#ff9715","#f20253","#677480"))+
  theme_complete_bw()+ylab("Percent")+xlab("")+
  facet_grid(~yr)+
  geom_text(mapping=aes(label=Mean.f,y=printmean),size=3,show.legend=FALSE) +
  theme(axis.text.x = element_text(size = 8),
        strip.text.x = element_text(size=14))
dev.off()


#-------------------------------------
# table
#-------------------------------------
loc.tab = vxloc %>% 
  dplyr::select(dist, loc, yr, Mean, lower, upper) %>%
  mutate(est = pt.est.ci.f(est = Mean, lb = lower, ub = upper, decimals = 1, scale=1)) %>%
  dplyr::select(-c(Mean, lower, upper))

tab.out = spread(loc.tab, key = yr, value = est)

write.csv(tab.out, file = paste0(tab_path, "tab-vxloc.csv"))






