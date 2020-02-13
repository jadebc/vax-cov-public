#!/bin/bash

R CMD BATCH 1-tab-vxcov.R
R CMD BATCH 2-fig-vxcov-rd.R
R CMD BATCH 3-fig-vxcov-race.R
R CMD BATCH 4-fig-vxcov-edu.R
R CMD BATCH 5-fig-vxcov-standardized-edu.R
R CMD BATCH 6-fig-vxcov-standardized-race.R
R CMD BATCH 7-fig-vxloc.R
R CMD BATCH 8-fig-vxtype.R
R CMD BATCH 9-fig-vxcov-participation.R

