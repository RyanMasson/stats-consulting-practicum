#### Implementing the gfoRmula package for preschooler implicit learning study
#### PSU CADES Consulting Lab / STAT 570: Statistical Consulting
#### Elizabeth “Betsy” Camp, Ryan Masson, Will McIntosh, Lauren Montefalcon

library(gfoRmula)
library(data.table)
setwd("~/Documents/psu/STAT 570/stat_consulting_preschoolers/modeling/")

testing = as.data.table(read.csv("gformula_prepped_data.csv"))

testing$trial = (testing$trial - 49) # settingindex column to required format 

testing$id = testing$participant # making explicitly named "id" column due to error

# covariates as factors
testing$group = as.factor(testing$group)
testing$sound_code = factor(testing$sound_code)
#testing$trialOrder = factor(testing$trialOrder) # issue with trialOrder as a factor

# buildiing the accuracy score column 
testing$accuracy = testing$correct
for (i in 2:nrow(testing)) {
  testing$accuracy[i] = testing$accuracy[i] + testing$accuracy[i-1]
  if(testing$trial[i] == 0) {
    testing$accuracy[i] = 0
  }
}

# logit transforming the outcome so it is continuous on the real line
offset <- 0.5
testing$accuracy_transf = log((testing$accuracy+offset)/(24-(testing$accuracy+offset)))
summary(testing$accuracy_transf)
hist(testing$accuracy_transf)

# constructing a cumulative alternations column
testing$cumulativeAlts = testing$changeTrial
for (i in 2:nrow(testing)) {
  testing$cumulativeAlts[i] = testing$cumulativeAlts[i] + testing$cumulativeAlts[i-1]
  if(testing$trial[i] == 0) {
    testing$cumulativeAlts[i] = 0
  }
}

# specifying the distributions of each time-varying covariate conditional on a function of history
covparams = list(covmodels = c(cumulativeAlts ~ lag1_cumulativeAlts +
                                 lag1_sound_code + group + trialOrder + trial,
                               sound_code ~ lag1_cumulativeAlts +
                                 lag1_sound_code + trialOrder + trial))

## model of outcome variable 
ymodel = as.formula(accuracy_transf ~ sound_code + cumulativeAlts + group + trialOrder)

## interventions
# intervening on: trialOrders
# simulates the counterfactuals wherein the participants were exposed to a different trialOrder
intvars = list(c("trialOrder"), c("trialOrder"), c("trialOrder"), 
                c("trialOrder"), c("trialOrder"), c("trialOrder"))
interventions = list(list(c(static, rep(1, 24))), list(c(static, rep(2, 24))),
                      list(c(static, rep(3, 24))), list(c(static, rep(4, 24))),
                      list(c(static, rep(5, 24))), list(c(static, rep(6, 24))))

# intervening on: sound_code
# simulates counterfactuals of being exposed to only one sound + maintaining either high/low and pitch/duration
# sound_code 0 (i.e., duration-high) —> 1
# sound_code 1 (i.e., duration-low) —>  2
# sound_code 2 (i.e., pitch-high) —> 3
# sound_code 3 (i.e., pitch-low) —>  4
#intvars = list(c("sound_code"), c("sound_code"), 
#               c("sound_code"), c("sound_code"),
#               c("sound_code"), c("sound_code"),
#               c("sound_code"), c("sound_code"))
#interventions = list(list(c(static, rep(1, 24))), list(c(static, rep(2, 24))),    # same sound
 #                    list(c(static, rep(3, 24))), list(c(static, rep(4, 24))),
#                     list(c(static, rep(c(1,2), 12))), list(c(static, rep(c(3,4), 12) )),  # maintain dimension
#                     list(c(static, rep(c(1,3), 12))), list(c(static, rep(c(2,4), 12) )))


# implement gfoRmula main function
mytime <- system.time(
  gform_implicit <- (gformula(obs_data = testing, 
                              outcome_type = "continuous_eof", 
                              outcome_name = "accuracy_transf", 
                              time_name = "trial",
                              time_points = 24,
                              id = "id", 
                              covnames = c("cumulativeAlts", "sound_code"),
                              covtypes = c("normal", "categorical"),
                              covparams = covparams,
                              basecovs = c("group", "trialOrder"),
                              histories = c(lagged), 
                              histvars = list(c("cumulativeAlts", "sound_code")),
                              intvars = intvars,
                              interventions = interventions,
                              int_descript = c("trialOrder 1", "trialOrder 2",
                                               "trialOrder 3", "trialOrder 4",
                                               "trialOrder 5", "trialOrder 6"),
                              #int_descript = c("Sound0", "Sound1",
                              #                 "Sound2", "Sound3",
                              #                 "Sound_Dur", "Sound_Pitch",
                              #                 "Sound_High", "Sound_Low"),
                              ymodel = ymodel,
                              seed = 1234,
                              nsamples = 100,
                              parallel = TRUE,
                              ncores = 7
  ))
)
print(mytime)


#extract coeffs and std.errs from accuracy_transf model
dplyr::tibble(names=names(gform_implicit$stderrs$accuracy_transf),
              coeff=gform_implicit$coeffs$accuracy_transf,
              std_err=gform_implicit$stderrs$accuracy_transf)


