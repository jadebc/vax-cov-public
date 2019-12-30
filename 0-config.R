##############################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# configure directories and load required libraries
##############################################
# load libraries
library(here)
library(plyr) 
library(assertthat)
library(dplyr)
library(tidyr)
library(reshape2)
library(sl3)
library(tlverse)
library(SuperLearner)
library(dplyr)
library(ggplot2)
library(gdata)
library(foreign)
library(plyr)
library(foreign)
library(Matching)

#---------------------------------------------
# Source base functions
#---------------------------------------------
source(paste0(here::here(),"/0-base-functions/0-base-functions.R"))
source(paste0(here::here(),"/0-base-functions/0-base-functions-tabfig.R"))
source(paste0(here::here(),"/0-base-functions/theme_complete_bw.R"))

#---------------------------------------------
# Paths to clean / processed datasets
#---------------------------------------------
# Ensure that data has been downloaded and saved in the 
# data path referenced below
data_path = paste0(here::here(), "/data/")

data_path_2017 = paste0(data_path, "vxcov_2017.csv")
data_path_2018 = paste0(data_path, "vxcov_2018.csv")

coverage_participation_data_path = paste0(data_path, "coverage_participation.csv")
district_demographics_path = paste0(data_path, "dist_demog.RData")

#---------------------------------------------
# Paths to results
#---------------------------------------------
results_path = paste0(here::here(), "/3-results/")

vax_results_2017_path = paste0(results_path, "vxcov_results.RData")
vax_results_2018_path = paste0(results_path, "vxcov_results_1718.RData")
vax_standardized_results_path = paste0(results_path, "vxcov_std_results.RData")
vax_school_results_path = paste0(results_path, "vxcov_school_results.RData")
vax_coverage_participation = paste0(results_path, "vxcov_results_coverage.RDS")

#---------------------------------------------
# Paths to saved figures and tables
#---------------------------------------------
plot_path = paste0(here::here(), "/4-figures/")
tab_path = paste0(here::here(), "/5-tables/")




