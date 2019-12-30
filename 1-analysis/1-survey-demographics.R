########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Obtain mean / percentage of demographic variables
########################################

# define directories, load libraries
source(here::here("0-config.R"))

#---------------------------------------------
# 2017 survey
#---------------------------------------------
data17 = read.csv(paste0(data_path_2017))
data17$dist=as.factor(data17$dist)

data17$edu=as.character(data17$edu)

data17 = data17 %>% mutate(college = case_when(
  edu == "Less than high school" ~ "No",
  edu == "High school" ~ "No", 
  edu == "Associate/College"~ "Yes",
  edu == "Postgrad" ~ "Yes",
  edu == "Missing" ~ "Missing/Error",
  edu == "Error" ~ "Missing/Error"
))

prop.table(table(data17$grade, data17$district),2)
prop.table(table(data17$language, data17$district),2)
prop.table(table(data17$college, data17$district),2)
prop.table(table(data17$race, data17$district),2)

#---------------------------------------------
# 2018 survey
#---------------------------------------------
data18 = read.csv(paste0(data_path_2018))

data18$edu=as.character(data18$edu)

data18 = data18 %>% mutate(college = case_when(
  edu == "Less than high school" ~ "No",
  edu == "High school" ~ "No", 
  edu == "Associate/College"~ "Yes",
  edu == "Postgrad" ~ "Yes",
  edu == "Missing" ~ "Missing/Error",
  edu == "Error" ~ "Missing/Error"
))

prop.table(table(data18$language, data18$district),2)
prop.table(table(data18$college, data18$district),2)
prop.table(table(data18$race, data18$district),2)
