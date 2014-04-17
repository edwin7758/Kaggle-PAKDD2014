library(rjson)
source("data_generation.R")
settings <- fromJSON(file="SETTINGS.json")

test <- loadData('test', settings)

#load learned parameters
allParams <- readRDS(paste(settings$PARAMETERS_PATH, 'modelParams.rds',sep=''))
beta_mc_age = allParams$par[["beta_mc_age"]]
beta_mct_scale = allParams$par[["beta_mct_scale"]]
beta_all_month = allParams$par[["beta_all_month"]]

#make predictions
mc <- test[,"moduleComponentID"]
mct <- test[,"moduleComponentSaletimeID"]  
a <- test[,"age"] + 1
month <- test[,'repairMonth']

inv.logit <- function(x)
{
  exp(x)/(1+exp(x))  
}

p <- inv.logit(
  beta_mct_scale[mct] +
  beta_mc_age[(a-1)*nrow(beta_mc_age) + mc] + # ie. beta_mc_age[mc,a]
  beta_all_month[month]
)
test[,'pred'] <- test[,'saleN'] * p
agg <- aggregate(test[,'pred'],by=list(test[,'moduleName'],test[,'componentName'],test[,'repairYear'],test[,'repairMonth']),FUN=sum)

#make submission
sub <- read.csv(paste(settings$DATA_PATH,'Output_TargetID_Mapping.csv',sep=''),stringsAsFactors=FALSE)
sub <- merge(sub, agg,by=1:4,all.x=TRUE)
sub[is.na(sub[,5]),5] <- 0
submission <- data.frame(id=1:nrow(sub),target=sub[,5])
write.csv(submission,paste(settings$SUBMISSIONS_PATH, "submission.csv",sep=''),quote=FALSE,row.names=FALSE)
