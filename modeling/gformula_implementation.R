#### Ryan Masson
#### PSU STAT 570, Spring 2023
#### Implementing gfoRmula for preschooler implicit learning study

library(gfoRmula)
library(data.table)
setwd("~/Documents/psu/STAT 570/stat_consulting_preschoolers/modeling/")


# set treatment levels to 4 or the full 12
levels = 4

#data = as.data.table(read.csv("clean_v2.csv")) 
#data = as.data.table(read.csv("scmm_prepped_data.csv"))
data = as.data.table(read.csv("gformula_prepped_data.csv"))
testing = data[data$phase == "explicit",] # above csv now already does this

## index column (time_name)
testing$trial = (testing$trial - 49)

# making explicitly named "id" column due to error 
testing$id = testing$participant

## treatment column, pitch/duration + sound file  (scmm_prepped_data already has this)
#testing$sound_label_original = testing$soundDimension
#for (i in 1:nrow(testing)) {
#  testing$sound_label_original[i] = paste(testing$soundDimension[i], testing$soundfileNum[i], sep = "_")
#}

## covariates as factors
testing$group = as.factor(testing$group)
#testing$trialOrder = as.factor(testing$trialOrder)
#testing$trialOrder = factor(testing$trialOrder)
# change sound_label_original to numeric (variable sound_code is now numeric)
#testing$sound_label_original = as.numeric(as.integer(testing$sound_label_original))

## outcome column / accuracy score (outcome_name)
testing$accuracy = testing$correct
for (i in 2:nrow(testing)) {
  testing$accuracy[i] = testing$accuracy[i] + testing$accuracy[i-1]
  if(testing$trial[i] == 0) {
    testing$accuracy[i] = 0
  }
}

# log transformation of accuracy
offset <- 0.5
testing$accuracy_transf = log((testing$accuracy+offset)/(24-(testing$accuracy+offset)))
summary(testing$accuracy_transf)
hist(testing$accuracy_transf)

## cumulative alternations across all trials (colinear with treatment, not using)
testing$cumulativeAlts = testing$changeTrial
for (i in 2:nrow(testing)) {
  testing$cumulativeAlts[i] = testing$cumulativeAlts[i] + testing$cumulativeAlts[i-1]
  if(testing$trial[i] == 0) {
    testing$cumulativeAlts[i] = 0
  }
}

## cumulative window of previous three trials (implemented by Betsy, not using)
testing$windowAlts = testing$changeTrial
for (i in 2:nrow(testing)) {
  if (testing$trial[i] == 0){
    testing$windowAlts[i] = 0
  }
  if (testing$trial[i] == 1){
    testing$windowAlts[i] = testing$changeTrial[i-1]
  }
  if (testing$trial[i] == 2){
    testing$windowAlts[i] = testing$changeTrial[i-1] + testing$changeTrial[i-2]
  }
  if (testing$trial[i] > 2) {
    testing$windowAlts[i] = testing$changeTrial[i-1] + testing$changeTrial[i-2] + 
      testing$changeTrial[i-3]
  }
}

testing$sum_changetrial_window[is.na(testing$sum_changetrial_window)] = 0

## covariate distributions (covtypes)
# specifies the distributions of each time-varying covariate conditional on a function of history
## ymodel
# takes an R model statement, passed to glm w/ gaussian family and identity link for continuous    eof
if (levels == 4) {
  covparams = list(covmodels = c(sum_changetrial_window ~ lag1_sum_changetrial_window +
                                   lag1_sound_code + group + trialOrder + trial,
                                 sound_code ~ lag1_sum_changetrial_window +                       
                                   lag1_sound_code + trialOrder + trial))
  ymodel = as.formula(accuracy_transf ~ sound_code + sum_changetrial_window + group + trialOrder)
  treatment_name = "sound_code"
} else {
  covparams = list(covmodels = c(sum_changetrial_window ~ lag1_sum_changetrial_window +
                                   lag1_sound_code_original + group + trialOrder + trial,
                                 sound_code_original ~ lag1_sum_changetrial_window +              
                                   lag1_sound_code_original + trialOrder + trial)) 
  ymodel = as.formula(accuracy_transf ~ sound_code_original + sum_changetrial_window + 
                        group + trialOrder)
  treatment_name = "sound_code_original"
}

## interventions
# intervention: random selection of sound_label_original vs. natural course
#intvars = c("sound_label_original")
#treatment_levels = unique(as.vector(testing$sound_label_original))
#int_random_treatment = list(c(static, sample(treatment_levels, 24, replace = TRUE)))
#interventions = list(int_random_treatment)

# intervention: trialOrders
intvars = list(c("trialOrder"), c("trialOrder"), c("trialOrder"), 
               c("trialOrder"), c("trialOrder"), c("trialOrder"))
interventions = list(list(c(static, rep(1, 24))), list(c(static, rep(2, 24))),
                     list(c(static, rep(3, 24))), list(c(static, rep(4, 24))),
                     list(c(static, rep(5, 24))), list(c(static, rep(6, 24))))

# implement gfoRmula main function
mytime <- system.time(
  gform_implicit <- (gformula(obs_data = testing, 
                           outcome_type = "continuous_eof", 
                           outcome_name = "accuracy_transf", 
                           time_name = "trial",
                           time_points = 24,
                           id = "id", 
                           covnames = c("sum_changetrial_window", treatment_name),
                           covtypes = c("normal", "categorical"),
                           covparams = covparams,
                           basecovs = c("group", "trialOrder"),
                           histories = c(lagged), 
                           histvars = list(c("sum_changetrial_window", treatment_name)),
                           intvars = intvars,
                           interventions = interventions,
                           int_descript = c("trialOrder 1", "trialOrder 2",
                                            "trialOrder 3", "trialOrder 4",
                                            "trialOrder 5", "trialOrder 6"),
                           ymodel = ymodel,
                           seed = 1234,
                           nsamples = 300,
                           parallel = TRUE,
                           ncores = 7
                          ))
)
print(mytime)

dplyr::tibble(names=names(gform_implicit$stderrs$accuracy_transf),
              coeff=gform_implicit$coeffs$accuracy_transf,
              std_err=gform_implicit$stderrs$accuracy_transf)

# output with 4 levels, no bootstrap

# output with 12 levels, no bootstrap

# output with 4 levels, 10 bootstrap samples
#> print(mytime)
#user  system elapsed 
#0.276   0.081   8.317 

# output with 12 levels, 10 bootstrap samples
#user  system elapsed 
#.270   0.082   8.363 

# output with 4 levels, 100 bootstrap samples
#user  system elapsed 
#0.330   0.154  28.761 

######################## DEBUGGING ####################################################
# frame 7, stats::weighted.mean
# obs_data[cur_time_ind][[covname]], w = w[prev_time_ind]
# issue with cumulativeAlts cov
# prev_time_ind is set to a empty logical?
# obs_data$id is NULL but it is being called
# id is not being assigned to "participant"
# going to try changing participant to id in the data