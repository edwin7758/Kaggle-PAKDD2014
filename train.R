library(rstan)
library(rjson)
source("data_generation.R")
settings <- fromJSON(file="SETTINGS.json")

#get data
train <- loadData('train', settings)
stan_data <- createStanData(train)

#compile stan model
model_file <- paste(settings$MODELS_PATH, "model.stan", sep='')
model <- stan_model(file=model_file)

#learn MAP parameters using specified initial values for the parameters
initParams <- list(
  beta_mct_start_mu = -10,
  beta_mct_start_sigma = 3,
  beta_mct_sigma = 1,
  beta_mct_scale = rep(-11,stan_data$nModuleComponentTimes),  
  mu_c = matrix(0,stan_data$nComponents,27),  
  mu_decay_c = matrix(-2,stan_data$nComponents,33), 
  alpha_mc = matrix(0,stan_data$nModuleComponents,27),  
  alpha_decay_mc = matrix(0,stan_data$nModuleComponents,33),  
  rho_mc = 0.5,
  rho_decay_mc = 0.5,
  beta_repairMonth11 = rep(0,11)
)
f <- optimizing(object=model, data=stan_data, iter=500, init=initParams, seed=1, as_vector=FALSE, algorithm='BFGS')

#save learned parameters to file
saveRDS(f, file=paste(settings$PARAMETERS_PATH, 'modelParams.rds', sep=''))    

