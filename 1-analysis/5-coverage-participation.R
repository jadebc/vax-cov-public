########################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage 

# Fit smoother between vaccination coverage
# and school participation in the intervention
########################################

rm(list = ls())
# define directories, load libraries
source(here::here("0-config.R"))

data = read.csv(file = coverage_participation_data_path)

#----------------------------------------------------
# function to predict Superlearner fit for each level of
# participation

# inputs:
# slfit: fit from Superlearner
# data: data from fed into Superlearner
# covars: covariates fed into Superlearner
# outcome: string for outcome variable name

# output: data frame with the predicted value for 
# each level of A
#----------------------------------------------------
predict_sl_A =  function(slfit, data, covars, outcome) {
  
  newdat = data 
  As <- unique(data$participation)
  pY <- rep(NA, length(As))
  for (i in 1:length(As)) {
    newdat$participation <- As[i]
    
    # this is required by prediction since it is added
    # by sl3
    newdat$delta_participation <- 0
    pred_task <- make_sl3_Task(
      data = newdat,
      covariates = c(covars,"delta_participation"),
      outcome = outcome
    )
    
    pYs <- slfit$predict(pred_task)
    
    pY[i] <- mean(pYs)
  }
  return(pY)
}

#----------------------------------------------------
# function to obtain Superlearner fit for each level of
# participation

# inputs:
# data: data from fed into Superlearner
# covars: covariates fed into Superlearner
# outcome: string for outcome variable name

# output: data frame with the predicted value for 
# each level of A
#----------------------------------------------------
fit_sl3 = function(data, outcome, covars){
  
  # create the main task
  main_task <- make_sl3_Task(
    data = data,
    covariates = covars,
    outcome = outcome
  )
  
  # choose base learners
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_mean <- make_learner(Lrnr_mean)
  lrnr_glmnet <- make_learner(Lrnr_glmnet)
  lrnr_xgboost <- make_learner(Lrnr_xgboost, nrounds=50, learning_rate = 0.01)
  
  lrnr_gam <- Lrnr_pkg_SuperLearner$new("SL.gam")
  lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")
  lrnr_stepAIC <- Lrnr_pkg_SuperLearner$new("SL.stepAIC")

  stack <- make_learner(
    Stack,
    lrnr_glm, lrnr_mean, lrnr_glmnet, lrnr_bayesglm,
    lrnr_bayesglm, lrnr_stepAIC, lrnr_gam
  )
  
  # define metalearner
  metalearner <- make_learner(Lrnr_nnls)
  
  # covariate screening 
  screen_cor <- Lrnr_pkg_SuperLearner_screener$new("screen.corP")
  
  # print selected covariates
  screen_cor$train(main_task)
  cor_pipeline <- make_learner(Pipeline, screen_cor, stack)
  
  # stack after pre-screening
  fancy_stack <- make_learner(Stack, cor_pipeline, stack)
  
  # make superlearner
  sl <- make_learner(Lrnr_sl,
                     learners = fancy_stack,
                     metalearner = metalearner
  )
  
  # train superlearner
  sl_fit <- sl$train(main_task)
  
  # obtain predicted values at each value of participation
  A_pred = predict_sl_A(slfit = sl_fit, 
                        data = data, 
                        covars = covars,
                        outcome = outcome)
  
  out = data.frame(
    schoolyr = unique(data$schoolyr),
    participation = unique(data$participation), 
    pred = A_pred
  )
  
  out = out %>% mutate(
    schoolyr = as.character(schoolyr)
  )
  
  return(out)
}

#----------------------------------------------------
# Run Superlearner
#----------------------------------------------------
data = data %>% rename(schoolyr = year)

data_list = list(
  y1 = data %>% filter(schoolyr=="2014-15"),
  y2 = data %>% filter(schoolyr=="2015-16"),
  y3 = data %>% filter(schoolyr=="2016-17"),
  y4 = data %>% filter(schoolyr=="2017-18")
)

# specify the outcome and covariates
covars <- colnames(data)[-which(names(data) %in% 
                                    c("schoolyr","school","absent_all","absent_ill","date","dist.n","dist",
                                      "week","yr","fluseasCDC","fluseasCDPH_2","fluseasCDPH_2.5",
                                      "fluseasCDPH_3","peakwk_2","peakwk_2.5","peakwk_3",
                                      "flusesn","matchid",
                                      "coverage","district","grades"))]
covars

set.seed(199999)
sl_pred = lapply(data_list, function(x) 
  fit_sl3(data = x, outcome = "coverage", covars = covars))

sl_pred_df = bind_rows(sl_pred)

sl_pred_df = sl_pred_df %>% mutate(
  participation = participation * 100,
  pred = pred *100
  )

saveRDS(sl_pred_df, vax_coverage_participation)

