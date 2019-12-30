#!/bin/bash

R CMD BATCH 1-survey-demographics.R  
R CMD BATCH 2-analyze.R
R CMD BATCH 3-analyze-1718.R
R CMD BATCH 4-standardize.R
R CMD BATCH 5-coverage-participation.R
