############################################
# Vaccine coverage survey conducted in March 2017
# RD with and without standardization
############################################

rm(list = ls())
# define directories, load libraries
source(here::here("0-config.R"))

# load("~/Dropbox/Flu/StFData/2016-2017/Data/Temp/vxcov_std_results.RData")
load(vax_standardized_results_path)


y1=as.data.frame(rbind(res.vx.y1,res.vx.y1.edu.dist,res.vx.y1.edu.dist.s,
         res.vx.y1.race.dist,res.vx.y1.race.dist.s))
y2=as.data.frame(rbind(res.vx.y2,res.vx.y2.edu.dist,res.vx.y2.edu.dist.s,
         res.vx.y2.race.dist,res.vx.y2.race.dist.s))
y3=as.data.frame(rbind(res.vx.y3,res.vx.y3.edu.dist,res.vx.y3.edu.dist.s,
         res.vx.y3.race.dist,res.vx.y3.race.dist.s))

y1.f = pt.est.ci.f(est = y1$pt.est, 
            lb = y1$lb,
            ub = y1$ub,
            decimals = 1,
            scale = 100)

y2.f = pt.est.ci.f(est = y2$pt.est, 
                   lb = y2$lb,
                   ub = y2$ub,
                   decimals = 1,
                   scale = 100)

y3.f = pt.est.ci.f(est = y3$pt.est, 
                   lb = y3$lb,
                   ub = y3$ub,
                   decimals = 1,
                   scale = 100)


tab.rd=as.data.frame(cbind(y1.f,y2.f,y3.f))
tab.rd$label=c("None","Education - Whole district","Education - Whole schools",
               "Race - Whole district","Race - Whole schools")
tab.rd=tab.rd[,c(4,1:3)]

save(tab.rd, file=paste0(tab_path,"tab-rd.RData"))
write.csv(tab.rd, file = paste0(tab_path,"tab-rd.csv"))
