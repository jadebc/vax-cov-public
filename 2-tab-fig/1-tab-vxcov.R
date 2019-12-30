########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Table: vaccination coverage by year
########################################
rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

# load y1-3 coverage
load(vax_results_2017_path)

# load y4 coverage
load(vax_results_2018_path)

# load difference results
load(vax_standardized_results_path)

rename_cols = function(data){
  data = data %>% rename(
    pt.est = Mean,
    lb = `Lower 95%CI`,
    ub = `Upper 95%CI`, )
  return(data)
}

vx.y1.o = rename_cols(vx.y1.o)
vx.y1.w = rename_cols(vx.y1.w)

vx.y2.o = rename_cols(vx.y2.o)
vx.y2.w = rename_cols(vx.y2.w)

vx.y3.o = rename_cols(vx.y3.o)
vx.y3.w = rename_cols(vx.y3.w)

vx.y4.o = rename_cols(vx.y4.o)
vx.y4.w = rename_cols(vx.y4.w)

ousd_res = data.frame(rbind(vx.y1.o, vx.y2.o, vx.y3.o, vx.y4.o))
wcc_res = data.frame(rbind(vx.y1.w, vx.y2.w, vx.y3.w, vx.y4.w))
diff_res=data.frame(rbind(res.vx.y1,res.vx.y2,res.vx.y3,res.vx.y4))


pt.est.ci.f = function(df,decimals,scale){
  df = as.data.frame(df)
  a=sprintf(paste("%0.0",decimals,"f",sep=""),df$pt.est*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),df$lb*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),df$ub*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}


tab = data.frame(
  ousd = pt.est.ci.f(df = ousd_res, decimals = 0, scale = 100),
  wcc = pt.est.ci.f(df = wcc_res, decimals = 0, scale = 100),
  diff = pt.est.ci.f(diff_res, decimals = 0, scale = 100)
)

tab = tab %>% mutate(yr=c("2014-15","2015-16","2016-17","2017-18")) %>%
  dplyr::select(yr, ousd, wcc, diff)

write.csv(tab, file = paste0(tab_path, "vaxcov_diff.csv"))

