##############################################
# Shoo the Flu Evaluation
# Absentee analysis
# 2014-2016

# compare results to other studies
##############################################
rm(list=ls())
library(ggplot2)
library(reshape2)
library(dplyr)
library(here)
library(RColorBrewer)
source(here("3-tab-fig", "theme_complete_bw.R"))

# ==============================================================================
# Global Setup
# ==============================================================================

cool_palette = colorRampPalette(brewer.pal(6, "PuBu"))
warm_palette = colorRampPalette(brewer.pal(6, "YlOrRd"))

palette_length = 30
chop_off_first = 5

cool_colors = cool_palette(palette_length)[chop_off_first:palette_length]
warm_colors = warm_palette(palette_length)[chop_off_first:palette_length]

palette_length = palette_length - chop_off_first

# fig_dir = here::here("..", "data", "flu", "vax-cov-figures/")
fig_dir = "~/Dropbox/Flu/StFData/Vax cov/Figures/"
# data_dir = here::here("..", "data", "flu", "vax-cov/")
data_dir = "~/Dropbox/Flu/StFData/Vax cov/"
# flu_res_dir = here::here("..", "data", "flu", "flu-results/")
flu_res_dir = "~/Dropbox/Flu/StFData/Vax cov/Figures/"

# Studies: a vector of all studies you want to map to colors (repeats allowed)
# Palette_length:
# grouping_regex:
# grouping_color:
# non_grouping_color:

studies_to_colors = function(studies, palette_len=palette_length, grouping_regex="Shoo the Flu", grouping_color=cool_colors, non_grouping_color=warm_colors, start_color_index = 1, start_color_increment=5) {
  unique_studies = studies %>% unique
  study_color_map = list()

  for (study in unique_studies) {
    
    if (grepl(pattern=grouping_regex, x=study))   assigned_colors = grouping_color
    else                                          assigned_colors = non_grouping_color
    
    study_color_map[[study]] = assigned_colors[start_color_index %% palette_len]
    start_color_index = start_color_index + start_color_increment
  }
  
  study_color_map = study_color_map %>% unlist
  return(study_color_map)
}

# ==============================================================================
# Vaccination coverage
# ==============================================================================

df = read.csv(paste0(data_dir,
                   "Figures/",
                   "School-located influenza vaccination literature summary - data.csv"))

colnames(df) = c("Study","Year","Season", "tr","Outcome","Outcome_desc","effect","age","type")

df$Year[df$Study=="Shoo the Flu"]=""

vx = df %>% filter(Outcome_desc=="vaccination coverage") %>%
  mutate(label=paste0(Study, ", ", Year, " (", Season,")")) %>%
  mutate(label=ifelse(Season=="",paste0(Study, ", ", Year),label))
  
# drop stepped wedge szilagyi
drops = which(vx$Study=="Szilagyi et al. (2015-16)" & vx$Year==2018)
vx = vx[-drops,]

stf_rows = grep("Shoo", vx$Study)

vx$label[stf_rows] = paste0(vx$Study[stf_rows], 
                            " (",
                            vx$Season[stf_rows],
                            ")")

vx$label[stf_rows] = paste0(vx$Study[stf_rows], 
                            " (",
                            vx$Season[stf_rows],
                            ")")

vx$label[vx$label=="Szilagyi et al. (2014-15), 2016 (2014-15)"] = "Szilagyi et al., 2016 (2014-15)"
vx$label[vx$label=="Szilagyi et al. (2015-16), 2017 (2015-16)"] = "Szilagyi et al., 2017 (2015-16)"


vx = vx %>% 
  mutate(label=ifelse(test=tr=="Intervention",
                      yes="",
                      no=label)) 

vx$size = c(rep(1,14),rep(1.2,8))

# Adding a color mapping that groups based on STF and Study
studies_to_colors_vx = studies_to_colors(studies=vx$Study)

# ==============================================================================
# Figure 1 - Vax Compare Studies No STF
# ==============================================================================

ggplot(data=vx, 
       mapping=aes(x=tr,
                   y=Outcome,
                   group=Study)) +
  # ----------------------------------------------------------------------------
  # Main plotting for figure
  geom_line(aes(color=Study, 
                size=size)) +
  
  geom_point(aes(color=Study, fill=Study),
             shape=21,
             alpha=0.6) +
  
  geom_text(mapping=aes(label=label,
                        fontface=c(rep(1, 14),
                                   rep(2, 8))),
            hjust=1.05,
            size=3,
            vjust=c(rep(0.5,8),
                    1.3,
                    rep(0.5,13))) +
  # ----------------------------------------------------------------------------
  # Herd immunity threshold
  geom_hline(yintercept=70,
             linetype="dotted") +
  
  geom_vline(xintercept=1:2, linetype="dashed", alpha=.75, color="grey", size = .25) +
  
  geom_text(x=2,
            y=72,
            aes(label="Estimated herd immunity threshold"),
            size=3) +
  # ----------------------------------------------------------------------------
  # Adjustments + Theming + Labels
  xlab("") +
  
  ylab("% Students vaccinated for influenza") +

  scale_size(range=c(0.5, 1.1), guide=FALSE) +
  
  scale_y_continuous(limits=c(0,100)) +
  
  scale_colour_manual(values=studies_to_colors_vx) + 
  
  theme_complete_bw() +

  theme(legend.position="none")
  # ----------------------------------------------------------------------------

ggsave(filename=paste(fig_dir, "vax-compare-studies.png"), width=7.5, height=7.5)

# ==============================================================================
# Figure 2 - Vax Compare Studies - No STF
# ==============================================================================

vx_no_stf = vx[-stf_rows,]

pdf(file="~/Dropbox/Flu/StFData/Vax cov/Figures/vax-compare-studies-nostf.pdf",
    width=10,height=6)

ggplot(vx_no_stf,aes(x=tr,y=Outcome,group=Study))+
  geom_line(aes(col=Study))+
  geom_point(aes(col=Study),shape=21,alpha=0.6)+
  geom_point(aes(fill=Study),shape=21,alpha=0.6)+
  geom_text(aes(label=label),hjust=1.05,size=3)+
  scale_y_continuous(limits=c(0,100))+
  theme_complete_bw()+ylab("% Students vaccinated for influenza")+
  xlab("")+geom_hline(yintercept=70,linetype="dotted")+
  geom_text(x=2,y=72,aes(label="Estimated herd immunity threshold"),size=3)

dev.off()


#---------------------------------------------
# absences all cause
# absences per 100 school days
#---------------------------------------------
abs=df %>% filter(Outcome_desc=="absence rate") %>%
  select(-c(effect, age, type))

# Kjos not included because they did not present means; 
# they just reported % of students with >=1 abs 

# drop Davis because they don't report results
# in the same format
abs = abs %>% filter(Study!="Davis et al.")

# shoo the flu
# load(paste0(flu_res_dir,
#             "tables/",
#             "unadj-table-raw.RData"))
load("~/Dropbox/Flu/StFData/Absentee/Results/unadj-table-raw.RData")

stf1w = data.frame(Study="Shoo the Flu Y1", Year="2014-15",
                  Season="2014-15",tr="Control",
                  Outcome=mn.abs.w$WCC[mn.abs.w$schoolyrp=="2014-15" & mn.abs.w$abstype=="mn_all"],
                  Outcome_desc="absence rate")
stf1o = data.frame(Study="Shoo the Flu Y1", Year="2014-15",
                  Season="2014-15",tr="Intervention",
                  Outcome=mn.abs.w$OUSD[mn.abs.w$schoolyrp=="2014-15" & mn.abs.w$abstype=="mn_all"],
                  Outcome_desc="absence rate")
stf2w = data.frame(Study="Shoo the Flu Y2", Year="2015-16",
                   Season="2015-16",tr="Control",
                   Outcome=mn.abs.w$WCC[mn.abs.w$schoolyrp=="2015-16" & mn.abs.w$abstype=="mn_all"],
                   Outcome_desc="absence rate")
stf2o = data.frame(Study="Shoo the Flu Y2", Year="2015-16",
                   Season="2015-16",tr="Intervention",
                   Outcome=mn.abs.w$OUSD[mn.abs.w$schoolyrp=="2015-16" & mn.abs.w$abstype=="mn_all"],
                   Outcome_desc="absence rate")
stf3w = data.frame(Study="Shoo the Flu Y3", Year="2016-17",
                   Season="2016-17",tr="Control",
                   Outcome=mn.abs.w$WCC[mn.abs.w$schoolyrp=="2016-17" & mn.abs.w$abstype=="mn_all"],
                   Outcome_desc="absence rate")
stf3o = data.frame(Study="Shoo the Flu Y3", Year="2016-17",
                   Season="2016-17",tr="Intervention",
                   Outcome=mn.abs.w$OUSD[mn.abs.w$schoolyrp=="2016-17" & mn.abs.w$abstype=="mn_all"],
                   Outcome_desc="absence rate")

abs_stf = rbind(abs, stf1o, stf1w,
                stf2o, stf2w,
                stf3o, stf3w)

abs_stf = abs_stf %>% 
  mutate(label=paste0(Study, ", ", Year)) %>%
  mutate(label=ifelse(tr=="Intervention","",label)) 
  

# abs$Control=c(6.63,4.2,4.40,0.597,0.0182,mn.y1.all.w*100,mn.y2.all.w*100)

# this is not DID, but
# because we know OUSD and WCCUSD had different
# absence rates before the program, 
# I subtracted the pre-program difference in 
# OUSD vs WCCUSD from the OUSD means

# abs$Intervention=c(4.34,3.9,3.055,0.203,0.0135,mn.y1.all.o*100-(pre.diff.all*100),mn.y2.all.o*100-(pre.diff.all*100))

# abs.l=melt(abs)
# abs.l$label=as.character(abs.l$Study)
# abs.l$label[abs.l$variable=="Control"]=""
# abs.l$Study=factor(abs.l$Study,levels=levels(abs.l$Study)[c(1:4,7,5:6)])

abs_stf$size = c(rep(1,8),rep(1.2,6))

# Adding a color mapping that groups based on STF and Study
studies_to_colors_abs = studies_to_colors(studies=abs_stf$Study)

# Retaining same colors from previous studies_to_colors() calls
studies_to_colors_abs = utils::modifyList(x=studies_to_colors_abs %>% as.list,
                                          val=studies_to_colors_vx %>% as.list) %>% unlist

# ==============================================================================
# Figure 3 - Absentee Studies
# ==============================================================================

ggplot(data=abs_stf,
       mapping=aes(x=tr,
                   y=Outcome,
                   group=Study)) +
  # ----------------------------------------------------------------------------
  # Main plotting for figure
  
  geom_point(aes(color=Study)) +
  
  geom_line(aes(color=Study,
                size=size)) +
  
  geom_text(aes(label=label,
                fontface=c(rep(1, 8),
                           rep(2, 6))),
            hjust=c(1.15, 
                    rep(1.1,13)),
            size=2.5,
            vjust=c(rep(0.5, 6), 
                    rep(0.5, 2),
                    rep(1.1, 2),
                    rep(0.5, 2),
                    rep(.5, 2))) +
  
  geom_vline(xintercept=1:2, linetype="dashed", alpha=.75, color="grey", size = .25) +
  # ----------------------------------------------------------------------------
  # Adjustments + Theming + Labels
  xlab("") +
  
  ylab("Mean absences per 100 days") +
  
  scale_size(range=c(0.5, 1.1), 
             guide=FALSE) +
  
  scale_y_continuous(limits=c(2,8), 
                     breaks=seq(2,8,0.5),
                     labels=sprintf("%0.1f",
                                    seq(2,8,0.5))) +
  
  scale_color_manual(values=studies_to_colors_abs) + 
  
  theme_complete_bw() +
  
  theme(legend.position="none")
# ----------------------------------------------------------------------------

ggsave(filename=paste(fig_dir, "absentee-studies.png"), width=6, height=6)

# ==============================================================================
# Figure 4 - Absentee Studies - No STF
# ==============================================================================

stf_rows = grep("Shoo", abs_stf$Study)
abs_no_stf = abs_stf[-stf_rows,]

pdf(file="~/Dropbox/Flu/StFData/Vax cov/Figures/absentee-studies-nostf.pdf",
    width=8,height=5)
ggplot(abs_no_stf,aes(x=tr,y=Outcome,group=Study))+
  geom_point(aes(col=Study))+geom_line(aes(col=Study))+
  theme_complete_bw()+ylab("Mean absences per 100 days")+
  geom_text(aes(label=label),hjust=1.1,size=2.5)+
  scale_y_continuous(limits=c(0,8),breaks=seq(0,8,0.5),
                     labels=sprintf("%0.1f",seq(0,8,0.5)))+ xlab("")
dev.off()

#---------------------------------------------
# absences all cause BY vaccination % change
# unadjusted DID absences per 100 school days
#---------------------------------------------
# load StF DID
ill.did=readRDS(paste0(flu_res_dir,
                       "5c-abs_glm_p2_adj_did_ill.RData"))
all.did=readRDS(paste0(flu_res_dir,
                       "5c-abs_glm_p2_adj_did_all.RData"))

absvx=data.frame(Study=c("King et al.","Pannaraj et al.",
                       "Davis et al.",
                       "Shoo the Flu Y1 - All","Shoo the Flu Y2 - All",
                       "Shoo the Flu Y3 - All",
                       "Shoo the Flu Y1 - Ill","Shoo the Flu Y2 - Ill",
                       "Shoo the Flu Y3 - Ill" ))

# these are just mean diffs; we can call did if we assume 
# that pre-prog rates were equal
king.did = abs$Outcome[abs$Study=="King et al." & abs$tr=="Intervention"] -
  abs$Outcome[abs$Study=="King et al." & abs$tr=="Control"]
mears.did = abs$Outcome[abs$Study=="Mears et al." & abs$tr=="Intervention"] -
  abs$Outcome[abs$Study=="Mears et al." & abs$tr=="Control"]
pannaraj.did = abs$Outcome[abs$Study=="Pannaraj et al." & abs$tr=="Intervention"] -
  abs$Outcome[abs$Study=="Pannaraj et al." & abs$tr=="Control"]
stf.y1.all = all.did$pt.est[all.did$year==1]
stf.y2.all = all.did$pt.est[all.did$year==2]
stf.y3.all = all.did$pt.est[all.did$year==3]
stf.y1.ill = ill.did$pt.est[ill.did$year==1]
stf.y2.ill = ill.did$pt.est[ill.did$year==2]
stf.y3.ill = ill.did$pt.est[ill.did$year==3]

absvx$vx=c(41,32,30,rep(c(-5,2,7),2))
absvx$meandiff=c(king.did,mears.did,pannaraj.did,
                 stf.y1.all*100,stf.y2.all*100,stf.y3.all*100,
                 stf.y1.ill*100,stf.y2.ill*100,stf.y3.ill*100)

# Adding a color column that is contingent upon the value of stf AND study
studies_to_colors_absvx = studies_to_colors(studies=absvx$Study)

# Retaining same colors from previous studies_to_colors() calls
studies_to_colors_absvx = utils::modifyList(x=studies_to_colors_absvx %>% as.list,
                                            val=studies_to_colors_vx %>% as.list)

studies_to_colors_absvx = utils::modifyList(x=studies_to_colors_absvx %>% as.list,
                                            val=studies_to_colors_abs %>% as.list) %>% unlist

# ==============================================================================
# Figure 5 - Absentee Studies VX
# ==============================================================================

ggplot(data=absvx,
       mapping=aes(x=vx,
                   y=meandiff,
                   group=Study)) +
  # ----------------------------------------------------------------------------
  # Main plotting for figure
  geom_point(aes(color=Study)) +
  
  geom_text(aes(label=Study),
            hjust=c(rep(1.1,3),
                    rep(c(-.1,-.1,-0.1), 2)),
            vjust=c(rep(0.4,4),
                    0.9,0.4,
                    rep(0.4,3)),
            size=2.5) +
  
  geom_vline(xintercept=seq(-10, 45, by=5), 
             linetype="dashed", 
             alpha=.75, 
             color="grey", 
             size = .25) +
  
  geom_hline(yintercept=seq(-2.5, .5, by=.5), 
             linetype="dashed", 
             alpha=.75, 
             color="grey", 
             size = .25) +
  
  geom_vline(xintercept=0) +

  geom_hline(yintercept=0) +
  
  # ----------------------------------------------------------------------------
  # Adjustments + Theming + Labels
  
  xlab("% vaccinated intervention - % vaccinated control") +
  
  ylab("Difference-in-difference of\nmean absences per 100 days") +
  
  scale_y_continuous(limits=c(-2.5,.5),
                     labels=seq(-2.5,.5,.5),
                     breaks=seq(-2.5,.5,.5)) +
  
  scale_x_continuous(limits=c(-10,45),
                     labels=seq(-10,45,5),
                     breaks=seq(-10,45,5)) + 
  
  scale_colour_manual(values=studies_to_colors_absvx) + 

  theme_complete_bw() +
  
  theme(legend.position="none")
# ------------------------------------------------------------------------------

ggsave(paste0(fig_dir, "absentee-studies-vx.png"), width=5, height=5)

# ==============================================================================
# Figure 6 - Absentee Studies VX Y3
# ==============================================================================

pdf(file="~/Dropbox/Flu/StFData/2016-2017/Results/absentee-studies-vx-y3.pdf",
    width=6,height=4)
ggplot(absvx,aes(x=vx,y=meandiff,group=Study))+
  geom_point(aes(col=Study))+
  theme_complete_bw()+ylab("Difference-in-difference of\nmean absences per 100 days")+
  geom_text(aes(label=Study),hjust=c(rep(1.1,3),-.1,-.1),
            vjust=c(rep(0.4,4),0.9),size=2.5)+
  scale_color_manual("",values=cols,guide=FALSE)+
  geom_vline(xintercept=0)+geom_hline(yintercept=0)+
  xlab("% vaccinated intervention - % vaccinated control")+
  scale_y_continuous(limits=c(-2.5,.5),labels=seq(-2.5,.5,.5),
                     breaks=seq(-2.5,.5,.5))+
  scale_x_continuous(limits=c(-10,45),labels=seq(-10,45,5),
                     breaks=seq(-10,45,5))+
  geom_vline(xintercept=8,col="#f20253",linetype="dashed")+
  annotate("text",x=15,y=-2,label="Shoo the Flu Y3\nvaccination coverage",
           col="#f20253",size=2.5)
dev.off()


pdf(file="~/Dropbox/Flu/StFData/2016-2017/Results/absentee-studies-vx-nostf.pdf",
    width=6,height=4)
ggplot(absvx[c(1:3),],aes(x=vx,y=meandiff,group=Study))+
  geom_point(aes(col=Study))+
  theme_complete_bw()+ylab("Difference-in-difference of\nmean absences per 100 days")+
  geom_text(aes(label=Study),hjust=c(rep(1.1,3)),
            vjust=c(rep(0.4,3)),size=2.5)+
  scale_color_manual("",values=cols,guide=FALSE)+
  geom_vline(xintercept=0)+geom_hline(yintercept=0)+
  xlab("% vaccinated intervention - % vaccinated control")+
  scale_y_continuous(limits=c(-2.5,.5),labels=seq(-2.5,.5,.5),
                     breaks=seq(-2.5,.5,.5))+
  scale_x_continuous(limits=c(-10,45),labels=seq(-10,45,5),
                     breaks=seq(-10,45,5))
dev.off()




#---------------------------------------------
# hospitalization / lab confirmed flu
# 1-RR x 100% where the RR is P(Y|E)/P(Y|U) 
# x axis is cont vs. int

# y axis is per 1000
#---------------------------------------------

# 
# surv=data.frame(Study=c(rep("King et al.",12),
#                 rep("Pebody et al. 2014-15",12),
#                 rep("Shoo the Flu Y1",2), rep("Shoo the Flu Y2",2),
#                 rep("Shoo the Flu Y1",2), rep("Shoo the Flu Y2",2),
#                 rep("Tran et al. Y1",2),rep("Tran et al. Y2",2),
#                 rep("Tran et al. Y1",2),rep("Tran et al. Y2",2),
#                 rep("Pebody et al. 2013-14",6)))
# # ADD Kjos + Pannaraj for outpatient
# 
# surv$type=c("Inpatient","Inpatient","Inpatient","Inpatient",
#             "Outpatient","Outpatient","Outpatient","Outpatient",
#             "ED/urgent care","ED/urgent care","ED/urgent care","ED/urgent care",
#             
#             rep(c("Outpatient","Outpatient","Inpatient","Inpatient"),3),
#             rep("Inpatient",4),rep("Inpatient",4),
#             rep("Outpatient ILI",8),
#             rep("Inpatient",6))
# 
# surv$y=c(2.7,1,2.0, 1.3,72.7,113.7,
#          49.6,67,10.3,13.2,8.9,9.7,
#          
#          2.669,.197,.177,.015,
#          2.531,.261,.5081,.2191,
#          .438,.128,.205,.069,
#          
#          0.407,0.479,0.325,0.335,
#          0.403,0.501,0.329,0.350,
#          
#          0.99, 5.59, 1.99,6.52,
#          1.68,16.06,3.44,20.99,
#          0.148,.314,.053,.07,.055,.07)
#   
# surv$x=c(rep(c("Intervention","Control"),6),
#          rep(c("Control","Intervention"),6),
#          rep(c("Intervention","Control"),4),
#          rep(c("Intervention","Control"),4),
#          rep(c("Intervention","Control"),3))
# 
# surv$x=factor(surv$x,levels=c("Control","Intervention"))
# surv$Effect=c(rep(c("Total","Total","Indirect","Indirect"),3),
#               
#               rep("Total",4),rep("Indirect",8),
#               rep("Total",4),rep("Indirect",4),
#               rep("Total",4),rep("Indirect",4),
#               rep("Indirect",4),rep("Total",2))
# 
# surv$age=c(rep(c("Elementary","Elementary","Adults","Adults"),3),
#            rep("Elementary",4),rep("Pre-school",4),rep("> Elementary",4),
#            rep("All ages",4),rep("Non-elem.",4),
#            rep("Elementary",4),rep("Pre-school",4),
#            rep("Pre-school",2),rep("> Elementary",2),rep("All ages",2))
# 
# surv$lab=paste(surv$Study, "-",surv$type,"-",surv$age)
# 
# surv$age=factor(surv$age,levels=c("Pre-school",
#       "Elementary","> Elementary","Adults","Non-elem.","All ages"))
# 
# surv$age.f=surv$age
# surv$age.f=as.character(surv$age.f)
# surv$age.f[surv$x=="Control"]=""
# # surv$age.f[surv$Study=="Shoo the Flu Y2"]=""

hosp = df %>% filter(Outcome_desc=="flu hospitalization")

plot.inp=surv[surv$type=="Inpatient" & surv$Effect!="Direct",]
plot.inp=plot.inp[order(plot.inp$Effect,plot.inp$Study,plot.inp$x),]
cols=c("#EE7722","#117FAA","#66AA55",
       "#EE3333","#992288")

#--- inpatient
pdf(file="~/Dropbox/Flu/StFData/2016-2017/Results/surv-rates-inpatient.pdf",
    width=8,height=5)
ggplot(plot.inp, aes(x=x,y=y,group=lab))+
  geom_point(aes(col=Study))+
  geom_line(aes(col=Study))+facet_wrap(~Effect)+
  ylab("Influenza hospitalization per 1000")+
  geom_text(aes(label=age.f),
    hjust=-0.15,vjust=c(0,0,0,1,0,1,rep(0,13),0.5,rep(0,4)),
        size=2.5)+
  xlab("")+theme_complete_bw()+
  scale_y_continuous(limits=c(0,3),breaks=seq(0,3,0.2),
                     labels=sprintf("%0.2f",seq(0,3,0.2)))+
  scale_color_manual(values=cols)

dev.off()

#--- inpatient no stf
pdf(file="~/Dropbox/Flu/StFData/2016-2017/Results/surv-rates-inpatient-nostf.pdf",
    width=8,height=5)
ggplot(plot.inp[(plot.inp$Study=="Pebody et al. 2013-14" | plot.inp$Study=="Pebody et al. 2014-15"|
        plot.inp$Study=="King et al.") & plot.inp$Effect!="Direct" & plot.inp$type=="Inpatient",],
       aes(x=x,y=y,group=lab))+
  geom_point(aes(col=Study))+
  geom_line(aes(col=Study))+facet_wrap(~Effect)+
  ylab("Influenza hospitalization per 1000")+
  geom_text(aes(label=age.f),
            hjust=-0.15,vjust=c(0,0,0,1,0,1,rep(0,9),0.5),
            size=2.5)+  xlab("")+theme_complete_bw()+
  scale_y_continuous(limits=c(0,3),breaks=seq(0,3,0.2),
                     labels=sprintf("%0.2f",seq(0,3,0.2)))+  
  scale_color_manual(values=cols)

dev.off()


#--- outpatient
ggplot(surv[surv$type=="Outpatient" & surv$Effect!="Direct",],
       aes(x=x,y=y,group=lab))+
  geom_point(aes(col=Study,shape=age))+
  geom_line(aes(col=Study))+facet_wrap(~Effect)+
  ylab("Outpatient influenza per 1000")
  # geom_text(aes(label=age.f),hjust=-0.1,size=2)



#---------------------------------------------
# hospitalization / lab confirmed flu
# 1-RR x 100% where the RR is P(Y|E)/P(Y|U)
# x axis is diff in vaccination
#---------------------------------------------


survvx=data.frame(Study=c("King","King","King",rep("Pebody 2014-15",6),
   "Tran Y1","Tran Y2","Shoo the Flu Y1","Shoo the Flu Y2",
   "Shoo the Flu Y1","Shoo the Flu Y2","Shoo the Flu Y1","Shoo the Flu Y2"))
survvx$vx=c(41,41,41,rep(18,6),30,30,-5,2,-5,2,-5,2)
y1.red.all=(1-(0.40687334/0.47912418))*100
y2.red.all=(1-(0.32498689/0.33496292))*100

y1.red.nonelem=(1-(0.3293641 /0.3498884))*100
y2.red.nonelem=(1-(0.4025561 /0.5011915))*100

y1.red.eld=(1-( 1.7215260  /1.7960415))*100
y2.red.eld=(1-(1.0099619 /1.3649916))*100

survvx$rd=c(-170,36,22,
         74,93,94,75,76,42,
         79,71,
         y1.red.all,y2.red.all,y1.red.nonelem,
         y2.red.nonelem,y1.red.eld,y2.red.eld)


survvx$Type=c("Inpatient","Outpatient","Outpatient",
     "Outpatient","Inpatient","Outpatient","Outpatient","Inpatient","Inpatient",
     "Outpatient","Outpatient",
     "Inpatient","Inpatient","Inpatient","Inpatient",
     "Inpatient","Inpatient")

survvx$Effect=c(rep("Direct",3),rep("Total",6),rep("Total",2),
              rep("Total",2),rep("Indirect",4))

survvx$Effect=factor(survvx$Effect,levels=c("Direct","Total","Indirect"))

cols=c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494')

survvx=survvx[survvx$Effect!="Direct",]

pdf(file="~/Dropbox/Flu/StFData/2016-2017/Results/survvx-studies.pdf",
    width=10,height=4)
ggplot(survvx,aes(x=cont,y=y,group=Study))+
  geom_point(aes(col=Study,shape=Type),
             alpha=0.6,size=3)+
  geom_point(aes(fill=Study,shape=Type),
             alpha=0.6,size=3)+
  theme_complete_bw()+ylab("% Reduction in outcome")+
  # geom_text(aes(label=Study),size=2.5)+
  scale_shape_manual("",values=c(21,24))+
  scale_fill_manual("",values=cols,guide=FALSE)+
  scale_color_manual("",values=cols)+
  geom_vline(xintercept=0)+geom_hline(yintercept=0)+
  xlab("% vaccinated control")+
  scale_y_continuous(limits=c(-10,100),labels=seq(-10,100,10),
                     breaks=seq(-10,100,10))+
  scale_x_continuous(limits=c(-10,100),labels=seq(-10,100,10),
                     breaks=seq(-10,100,10))+
  facet_wrap(~Effect)
dev.off()

pdf(file="~/Dropbox/Flu/StFData/2016-2017/Results/survvx-studies-vx.pdf",
    width=10,height=4)
ggplot(survvx,aes(x=vx,y=y,group=Study))+
  geom_point(aes(col=Study,shape=Type),
      alpha=0.6,size=3)+
  geom_point(aes(fill=Study,shape=Type),
             alpha=0.6,size=3)+
  theme_complete_bw()+ylab("% Reduction in outcome")+
  # geom_text(aes(label=Study),size=2.5)+
  scale_shape_manual("",values=c(21,24))+
  scale_fill_manual("",values=cols,guide=FALSE)+
  scale_color_manual("",values=cols)+
  geom_vline(xintercept=0)+geom_hline(yintercept=0)+
  xlab("% vaccinated intervention - % vaccinated control")+
  scale_y_continuous(limits=c(-10,100),labels=seq(-10,100,10),
                     breaks=seq(-10,100,10))+
  scale_x_continuous(limits=c(-10,70),labels=seq(-10,70,10),
                     breaks=seq(-10,70,10))+
  facet_wrap(~Effect)
dev.off()


pdf(file="~/Dropbox/Flu/StFData/2016-2017/Results/survvx-studies-vx-nostf.pdf",
    width=10,height=4)
ggplot(survvx[survvx$Study!="Shoo the Flu Y1" & survvx$Study!="Shoo the Flu Y2",],
       aes(x=vx,y=y,group=Study))+
  geom_point(aes(col=Study,shape=Type),
             alpha=0.6,size=3)+
  geom_point(aes(fill=Study,shape=Type),
             alpha=0.6,size=3)+
  theme_complete_bw()+ylab("% Reduction in outcome")+
  # geom_text(aes(label=Study),size=2.5)+
  scale_shape_manual("",values=c(21,24))+
  scale_fill_manual("",values=cols,guide=FALSE)+
  scale_color_manual("",values=cols)+
  geom_vline(xintercept=0)+geom_hline(yintercept=0)+
  xlab("% vaccinated intervention - % vaccinated control")+
  scale_y_continuous(limits=c(-10,100),labels=seq(-10,100,10),
                     breaks=seq(-10,100,10))+
  scale_x_continuous(limits=c(-10,70),labels=seq(-10,70,10),
                     breaks=seq(-10,70,10))+
  facet_wrap(~Effect)
dev.off()
