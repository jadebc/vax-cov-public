########################################
# Vaccine coverage survey conducted in March 2017
# Individual child trajectories over years
# Table and figures
########################################

rm(list=ls())
library(reshape2)
library(ggplot2)
source("~/Documents/CRG/flu/vax-cov/3-tab-fig/theme_complete_bw.R")

plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"
tab.dir="~/Dropbox/Flu/StFData/Vax cov/Tables/"

load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov.RData")
source("~/Documents/CRG/flu/vax-cov/2-analysis/0-base-functions.R")
data$dist=as.factor(data$dist)
data$schoolname=droplevels(data$schoolname)

# create indicator with 1=vacinated, 0=not vaccinated, error, missing
data$vx1415yn=ifelse(data$vx1415==1,1,0)
data$vx1516yn=ifelse(data$vx1516==1,1,0)
data$vx1617yn=ifelse(data$vx1617==1,1,0)

# create indicators for whether the child shifted from unvac to vac, 
# did not shift, or shited from vac to unvac
data$y12_inc=ifelse(data$vx1415yn==0 & data$vx1516yn==1,1,0)
data$y12_same=ifelse((data$vx1415yn==0 & data$vx1516yn==0)|
                     (data$vx1415yn==1 & data$vx1516yn==1),1,0)
data$y12_dec=ifelse(data$vx1415yn==1 & data$vx1516yn==0,1,0)
data$y12_shift[data$y12_inc==1]="Unvaccinated\nto vaccinated"
data$y12_shift[data$y12_same==1]="No change"
data$y12_shift[data$y12_dec==1]="Vaccinated to\nunvaccinated"

data$y23_inc=ifelse(data$vx1516yn==0 & data$vx1617yn==1,1,0)
data$y23_same=ifelse((data$vx1516yn==0 & data$vx1617yn==0)|
                     (data$vx1516yn==1 & data$vx1617yn==1),1,0)
data$y23_dec=ifelse(data$vx1516yn==1 & data$vx1617yn==0,1,0)
data$y23_shift[data$y23_inc==1]="Unvaccinated\nto vaccinated"
data$y23_shift[data$y23_same==1]="No change"
data$y23_shift[data$y23_dec==1]="Vaccinated to\nunvaccinated"

data$y13_inc=ifelse(data$vx1415yn==0 & data$vx1617yn==1,1,0)
data$y13_same=ifelse((data$vx1415yn==0 & data$vx1617yn==0)|
                     (data$vx1415yn==1 & data$vx1617yn==1),1,0)
data$y13_dec=ifelse(data$vx1415yn==1 & data$vx1617yn==0,1,0)
data$y13_shift[data$y13_inc==1]="Unvaccinated\nto vaccinated"
data$y13_shift[data$y13_same==1]="No change"
data$y13_shift[data$y13_dec==1]="Vaccinated to\nunvaccinated"

y12=melt(prop.table(table(data$y12_shift,data$dist),2))
y23=melt(prop.table(table(data$y23_shift,data$dist),2))
y13=melt(prop.table(table(data$y13_shift,data$dist),2))

# make graph
graphdf=data.frame(rbind(y12,y23,y13))
colnames(graphdf)=c("x","District","y")
graphdf$yr=c(rep("Year 1-2",6),rep("Year 2-3",6),
             rep("Year 1-3",6))
graphdf$ylab=sprintf("%0.0f",graphdf$y*100)
graphdf$y=graphdf$y*100

graphdf$yr=factor(graphdf$yr,levels=c("Year 1-2",
    "Year 2-3","Year 1-3"))
graphdf$x=factor(graphdf$x,levels=c("Vaccinated to\nunvaccinated",
  "No change","Unvaccinated\nto vaccinated"))

pdf(file=paste0(plot.dir,"fig-vxcov-shift.pdf"),
    width=11,height=4)
ggplot(graphdf,aes(x=x,y=y,group=District))+
  geom_bar(aes(fill=District),stat="identity",
           position=position_dodge(width=0.9))+
geom_text(mapping=aes(x=x,y=y,label=ylab),vjust=-0.3,
         position=position_dodge(0.9),show.legend=FALSE,size=3)+
  facet_wrap(~yr)+theme_complete_bw()+
  ylab("Percent vaccinated for influenza")+xlab("")+
  scale_y_continuous(limits=c(0,100))+
  scale_fill_manual("",values=c("#2185c5","#ff9715"))
dev.off()

# make table
colnames(y13)=c("type","dist","value")

tab=c(y13[3,3],y13[6,3],y13[1,3],y13[4,3],y13[2,3],
      y13[5,3])

#-----------------------------------------
# make table that stratifies by race 
#-----------------------------------------
y12.race=aggregate(data[,c("y12_dec","y12_same","y12_inc")],by=list(data$dist,data$race),mean)
y23.race=aggregate(data[,c("y23_dec","y23_same","y23_inc")],by=list(data$dist,data$race),mean)
y13.race=aggregate(data[,c("y13_dec","y13_same","y13_inc")],by=list(data$dist,data$race),mean)

colnames(y12.race)[3:5]=c("Decrease","Same","Increase")
colnames(y23.race)[3:5]=c("Decrease","Same","Increase")
colnames(y13.race)=c("dist","race","Decrease","Same","Increase")

# make table
tab.race=reshape(y13.race, idvar = "race", timevar = "dist", direction = "wide")
tab.race=tab.race[,c("race","Decrease.OUSD","Decrease.WCCUSD",
                     "Same.OUSD","Same.WCCUSD","Increase.OUSD","Increase.WCCUSD")]

#dropping race categories with very few obs (<20 per cell)
tab.race=tab.race[tab.race$race!="Native American" & tab.race$race!="Pacific islander"&
                    tab.race$race!="Missing",]
for(i in 2:7){
  tab.race[,i]=sprintf("%0.0f",tab.race[,i]*100)
}
tab.race$race=as.character(tab.race$race)
tab.race$race[tab.race$race=="Black"]="African American"
tab.race$race[tab.race$race=="Multi"]="Multiple races"

# make graph - unfinished
graph.race=melt(y13.race,id.vars=c("dist","race"))
graph.race=graph.race[graph.race$race!="Native American" & graph.race$race!="Pacific Islander"&
                    graph.race$race!="Missing",]

ggplot(graph.race,aes(x=variable,y=value,group=race))+
  geom_bar(aes(fill=dist),alpha=0.5,stat="identity",color="black",
           position=position_dodge(width=0.9))+
  scale_fill_manual(values=c("#001DFC","#FCF800"))

#-----------------------------------------
# make table that stratifies by parent education 
#-----------------------------------------
y12.edu=aggregate(data[,c("y12_dec","y12_same","y12_inc")],by=list(data$dist,data$edu),mean)
y23.edu=aggregate(data[,c("y23_dec","y23_same","y23_inc")],by=list(data$dist,data$edu),mean)
y13.edu=aggregate(data[,c("y13_dec","y13_same","y13_inc")],by=list(data$dist,data$edu),mean)

colnames(y12.edu)[3:5]=c("Decrease","Same","Increase")
colnames(y23.edu)[3:5]=c("Decrease","Same","Increase")
colnames(y13.edu)=c("dist","edu","Decrease","Same","Increase")

# make table
tab.edu=reshape(y13.edu, idvar = "edu", timevar = "dist", direction = "wide")
tab.edu=tab.edu[,c("edu","Decrease.OUSD","Decrease.WCCUSD",
                     "Same.OUSD","Same.WCCUSD","Increase.OUSD","Increase.WCCUSD")]

#dropping edu categories with very few obs (<20 per cell)
tab.edu=tab.edu[tab.edu$edu!="Error",]
for(i in 2:7){
  tab.edu[,i]=sprintf("%0.0f",tab.edu[,i]*100)
}

# make graph - unfinished
graph.edu=melt(y13.edu,id.vars=c("dist","edu"))
graph.edu=graph.edu[graph.edu$edu!=6,]
graph.edu$variable=as.character(graph.edu$variable)
graph.edu$variable[graph.edu$variable=="Decrease"]="Vaccinated to unvaccinated"
graph.edu$variable[graph.edu$variable=="Increase"]="Unvaccinated to vaccinated"
graph.edu$variable[graph.edu$variable=="Same"]="No change"
graph.edu$variable=factor(graph.edu$variable,levels=c("Vaccinated to unvaccinated",
                                                          "No change","Unvaccinated to vaccinated"))
graph.edu$value=graph.edu$value*100
colnames(graph.edu)[1]="District"
graph.edu=graph.edu[graph.edu$edu!="Error",]
graph.edu$edu=as.character(graph.edu$edu)
graph.edu$edu[graph.edu$edu=="Less than high school"]="Less than HS"
graph.edu$edu[graph.edu$edu=="High school"]="HS"
graph.edu$edu[graph.edu$edu=="Associate/College"]="College"
graph.edu$edu=factor(graph.edu$edu,levels=c("Less than HS","HS","College","Postgrad"))

xseq=c(c(0.63+0.2*seq(0,3)),c(1.63+0.15*seq(0,3)),c(2.63+0.15*seq(0,3)))

ggplot(graph.edu,aes(x=variable,y=value,group=edu))+
  geom_bar(aes(fill=District),alpha=0.5,stat="identity",color="black",
           position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(-18, 80), expand = FALSE) +
  scale_fill_manual(values=c("#001DFC","#FCF800"))+
  annotate(geom = "text", x = c(1,2,3), y = -15, label = unique(graph.edu$variable),
           size = 3.5) +
  annotate(geom = "text", x = xseq, y = -3, label = rep(unique(graph.edu$edu),3), size = 3) +
  theme_bw()+ylab("Percent")+
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank())


#-----------------------------------------
# make table that stratifies by grade
#-----------------------------------------
y12.grade=aggregate(data[,c("y12_dec","y12_same","y12_inc")],by=list(data$dist,data$grade),mean)
y23.grade=aggregate(data[,c("y23_dec","y23_same","y23_inc")],by=list(data$dist,data$grade),mean)
y13.grade=aggregate(data[,c("y13_dec","y13_same","y13_inc")],by=list(data$dist,data$grade),mean)

colnames(y12.grade)[3:5]=c("Decrease","Same","Increase")
colnames(y23.grade)[3:5]=c("Decrease","Same","Increase")
colnames(y13.grade)=c("dist","grade","Decrease","Same","Increase")

tab.grade=reshape(y13.grade, idvar = "grade", timevar = "dist", direction = "wide")
tab.grade=tab.grade[,c("grade","Decrease.OUSD","Decrease.WCCUSD",
                   "Same.OUSD","Same.WCCUSD","Increase.OUSD","Increase.WCCUSD")]
tab.grade$grade[tab.grade$grade==0]="K"
#dropping 6th grade
tab.grade=tab.grade[tab.grade$grade!="6",]

for(i in 2:7){
  tab.grade[,i]=sprintf("%0.0f",tab.grade[,i]*100)
}

# make graph - unfinished
graph.grade=melt(y13.grade,id.vars=c("dist","grade"))
graph.grade=graph.grade[graph.grade$grade!=6,]
graph.grade$variable=as.character(graph.grade$variable)
graph.grade$variable[graph.grade$variable=="Decrease"]="Vaccinated to unvaccinated"
graph.grade$variable[graph.grade$variable=="Increase"]="Unvaccinated to vaccinated"
graph.grade$variable[graph.grade$variable=="Same"]="No change"
graph.grade$variable=factor(graph.grade$variable,levels=c("Vaccinated to unvaccinated",
        "No change","Unvaccinated to vaccinated"))
graph.grade$value=graph.grade$value*100
colnames(graph.grade)[1]="District"

xseq=c(c(0.63+0.15*seq(0,5)),c(1.63+0.15*seq(0,5)),c(2.63+0.15*seq(0,5)))
  
ggplot(graph.grade,aes(x=variable,y=value,group=grade))+
  geom_bar(aes(fill=District),alpha=0.5,stat="identity",color="black",
           position=position_dodge(width=0.9))+
  coord_cartesian(ylim = c(-18, 80), expand = FALSE) +
  scale_fill_manual(values=c("#001DFC","#FCF800"))+
  annotate(geom = "text", x = c(1,2,3), y = -9, label = unique(graph.grade$variable),
           size = 3.5) +
  annotate(geom = "text", x = xseq, y = -3, label = rep(seq(0,5,1),3), size = 3) +
  theme_bw()+ylab("Percent")+
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank())
  

save(tab,tab.race,tab.edu,tab.grade,file=paste0(tab.dir,"tab-traj.RData"))

