##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Trying to make a map
##########################################
rm(list=ls())
library(rgdal)
library(maps)
library(ggmap)
library(viridis)

plot.dir="~/Dropbox/Flu/StFData/Vax cov/Figures/"

# #------------------------------------------
# # map of county boundaries
# #------------------------------------------
# states <- map_data("state")
# ca <- subset(states, region %in% c("california"))
# 
# counties <- map_data("county")
# ca_county <- subset(counties, region == "california")
# 
# bay <- subset(ca_county, subregion=="alameda"|subregion=="contra costa"|
#     subregion=="san francisco"|subregion=="san mateo"|subregion=="santa clara")
# 
# ggplot(data = bay) + 
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
#   coord_fixed(1.3)

#------------------------------------------
# read in school lat long
# NEEDS FURTHER REDUCTION TO OUR STUDY SUBSET
# https://www.cde.ca.gov/ds/si/ds/pubschls.asp
#------------------------------------------
sll=read.csv("~/Dropbox/Flu/StFData/GIS/OUSD shape files/pubschls.csv")
ll=sll[sll$District=="Oakland Unified" | sll$District=="West Contra Costa Unified",]
ll=ll[ll$SOCType=="Elementary Schools (Public)",]
ll=ll[ll$GSoffered=="K-5"|ll$GSoffered=="K-6",]

ll_bbox <- make_bbox(lat = Latitude, lon = Longitude, data = ll)
ll_big <- get_map(location = ll_bbox, source = "google", maptype = "terrain",zoom=11)

ggmap(ll_big) + 
  geom_point(data = ll, mapping = aes(x = Longitude, y = Latitude, fill = District),colour="black",shape=21,size=3)+
  scale_fill_manual("",values=c("#0066cc","#009933"))+
  xlab("")+ylab("")+
  theme(legend.position="bottom")

# our vx cov svy data
load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_school_results.RData")

colnames(ll)[which(colnames(ll)=="School")]="schoolname"
colnames(ll)[which(colnames(ll)=="District")]="dist"
ll$dist=as.character(ll$dist)
ll$dist[ll$dist=="Oakland Unified"]="OUSD"
ll$dist[ll$dist=="West Contra Costa Unified"]="WCCUSD"
ll$schoolname[ll$schoolname=="Martin Luther King, Jr. Elementary"]="King Elementary"

vx.y1=merge(vx.y1.school,ll[,c("schoolname","dist","Latitude","Longitude")],
        by=c("schoolname","dist"),all.x=TRUE)

vx.y2=merge(vx.y2.school,ll[,c("schoolname","dist","Latitude","Longitude")],
            by=c("schoolname","dist"),all.x=TRUE)

vx.y3=merge(vx.y3.school,ll[,c("schoolname","dist","Latitude","Longitude")],
            by=c("schoolname","dist"),all.x=TRUE)

vx.y4=merge(vx.y4.school,ll[,c("schoolname","dist","Latitude","Longitude")],
            by=c("schoolname","dist"),all.x=TRUE)

vx.y1$yr="2014-15"
vx.y2$yr="2015-16"
vx.y3$yr="2016-17"
vx.y4$yr="2017-18"

vx=rbind(vx.y1,vx.y2,vx.y3, vx.y4)
vx$Mean=vx$Mean*100
vx = vx[!is.na(vx$dist),]

vx_bbox <- make_bbox(lat = Latitude, lon = Longitude, data = vx)
vx_big <- get_map(location = ll_bbox, source = "google", maptype = "terrain",zoom=11)

pdf(file=paste0(plot.dir,"map-school-coverage.pdf"),width=10,height=4)
ggmap(vx_big) + 
  geom_point(data = vx, mapping = aes(x = Longitude, y = Latitude, fill = Mean, shape=dist),
          colour="black",size=2)+
  scale_shape_manual("",values=c(23,21))+
  scale_fill_gradientn("Percent vaccinated for influenza",colours = rev(terrain.colors(10)))+
  xlab("")+ylab("")+
  theme(legend.position="bottom")+
  facet_grid(~yr)+
  theme(    strip.background = element_rect(fill = NA, colour = NA), 
            strip.text.x =     element_text(colour = "black", size = 10, face="bold"),
            strip.text.y =     element_text(colour = "black", size = 10, angle = -90,vjust=-50),
            axis.text.x=element_blank(),axis.ticks.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks.y=element_blank())
dev.off()

