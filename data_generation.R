#add sale count to the original Kaggle dataset for given month&module
addSalesInformation <- function(salesData, year, month, module, saleN)
{
  tmp <- unique(salesData[salesData[,1]==module,1:2])
  tmp[,3] <- paste(year,'/',month,sep='')
  tmp[,4] <- saleN
  names(tmp) <- names(salesData)
  return(rbind(salesData,tmp))
}

#return month from date string in the format year/month
getMonth <- function(data)
{
  return(as.numeric(sub("^[0-9]+/([0-9]+)", "\\1", data)))
}

#return year from date string in the format year/month
getYear <- function(data)
{
  return(as.numeric(sub("^([0-9]+)/[0-9]+", "\\1", data)))
}

#return distance (measured in months) from date minYear/1
changeDateStringsToNumeric <- function(data, minYear=2005)
{
  tmp <- data.frame(year=getYear((data)), month=getMonth((data)))
  tmp[,1] <- tmp[,1] - minYear
  return(12*tmp[,1] + tmp[,2])
}

#adds all missing zero repair months to the dataset
#slow, should be refactored
addAllPossibleRepairMonths <- function(data, maxYear, maxMonth)
{ 
  dates <- c(NA,(maxYear-2005-1)*12 + maxMonth)
  ind <- 1
  for(i in 2005:maxYear)
  {
    for(j in 1:12)
    {
      if(i!=maxYear | (i==maxYear & j<=maxMonth))
      {
        dates[ind] <- paste(i,'/',j,sep='')
        ind <- ind +1
      }
    }
  }  
  N <- length(dates)
  tmp <- data.frame(matrix(NA, nrow = 300000, ncol = 5))
  
  for(i in 1:nrow(data))
  {
    allPossibilities <- dates[(which(dates == data[i,3])):N]
    ind_s <- min(which(is.na(tmp[,1])))
    ind_e <- ind_s + length(allPossibilities) - 1
    tmp[ind_s:ind_e,] <- data.frame(data[i,1],data[i,2],data[i,3],allPossibilities,data[i,4], stringsAsFactors=FALSE)
  }
  return(tmp[!is.na(tmp[,1]),])
}

#creates unique ID numbers for given data frame values
createIDForUniqueValueCombos <- function(data)
{
  data <- as.data.frame(data)
  map <- data.frame(unique(data))
  if(ncol(map)>1)
    map <- map[do.call(order, map),]
  tmp <- rep(0,length(data))
  for(i in 1:nrow(map))
  {
    belongs <- rep(TRUE,nrow(data))
    for(j in 1:ncol(map))
      belongs <- belongs & data[,j]==map[i,j]
    tmp[belongs] <- i
  }
  return(tmp)
}

#the main function to create the custom datasets used by train/predict.R
#removes moduleComponents if there was less than repairN_cut repairs in the dataset to reduce the number of required parameters
createDataset <- function(settings, repairN_cut=20, lastYear=2011, lastMonth=7)
{
  repairData <- read.csv(settings$KAGGLE_REPAIR_DATA_PATH, stringsAsFactors=FALSE)
  salesData <- read.csv(settings$KAGGLE_SALE_DATA_PATH, stringsAsFactors=FALSE)
  
  #simple imputation for missing sales data
  salesData <- addSalesInformation(salesData,2006,5,"M7",50000)
  
  aggSales <- aggregate(salesData[,4],by=list(salesData[,1],salesData[,2],salesData[,3]),FUN=sum)
  aggSales <- aggSales[aggSales[,4] > 0,]
  aggRepair <- aggregate(repairData[,5],by=list(repairData[,1],repairData[,2],repairData[,3],repairData[,4]),FUN=sum)
  
  suitableMC <- aggregate(repairData[,5],by=list(repairData[,1],repairData[,2]),FUN=sum)
  suitableMC <- suitableMC[suitableMC[,3]>=repairN_cut,]
  suitableMC <- paste(suitableMC[,1],suitableMC[,2])
  
  aggSales <- aggSales[paste(aggSales[,1],aggSales[,2]) %in% suitableMC,] 
  aggRepair <- aggRepair[paste(aggRepair[,1],aggRepair[,2]) %in% suitableMC,] 
  
  tmp <- addAllPossibleRepairMonths(aggSales, lastYear, lastMonth)
  tmp <- merge(aggRepair,tmp,by.x=c(1:4),by.y=c(1:4),all.y=TRUE)
  tmp[is.na(tmp[,5]),5] <- 0 
  
  haveBeenRepaired <- (aggregate(tmp[,5],by=list(tmp[,1],tmp[,2]),FUN=sum))
  haveBeenRepaired <- paste(haveBeenRepaired[haveBeenRepaired[,3]>0,1], haveBeenRepaired[haveBeenRepaired[,3]>0,2])
  tmp <- tmp[paste(tmp[,1],tmp[,2]) %in% haveBeenRepaired,]
  names(tmp) <- c("moduleName","componentName","stringSaleDate", "stringRepairDate", "repairN", "saleN")
  
  tmp[,7] <- getYear(tmp[,"stringSaleDate"])
  tmp[,8] <- getMonth(tmp[,"stringSaleDate"])
  tmp[,9] <- getYear(tmp[,"stringRepairDate"])
  tmp[,10] <- getMonth(tmp[,"stringRepairDate"])
  names(tmp) <- c(names(tmp)[1:6],"saleYear","saleMonth","repairYear","repairMonth")  
  
  tmp[,11] <- createIDForUniqueValueCombos(tmp[,"moduleName"])
  tmp[,12] <- createIDForUniqueValueCombos(tmp[,"componentName"])
  tmp[,13] <- createIDForUniqueValueCombos(tmp[,c("moduleName","componentName")])
  tmp[,14] <- createIDForUniqueValueCombos(tmp[,c("moduleName","componentName","saleYear","saleMonth")])  
  names(tmp) <- c(names(tmp)[1:10],"moduleID","componentID", "moduleComponentID", "moduleComponentSaletimeID")
  
  
  tmp[,15] <- changeDateStringsToNumeric(tmp[,"stringSaleDate"])
  tmp[,16] <- changeDateStringsToNumeric(tmp[,"stringRepairDate"])
  tmp[,17:18] <- 0
  for(i in 1:max(tmp$moduleID))
  {
    tmp[tmp$moduleID==i,17] <- tmp[tmp$moduleID==i,15] - min(tmp[tmp$moduleID==i,15])
    tmp[tmp$moduleID==i,18] <- tmp[tmp$moduleID==i,16] - min(tmp[tmp$moduleID==i,15])
  }
  names(tmp) <- c(names(tmp)[1:14],"unnormalizedSaleTime","unnormalizedRepairTime","normalizedSaleTime","normalizedRepairTime") 
  
  tmp[,19] <- tmp[,16] - tmp[,15]
  tmp[,20] <- changeDateStringsToNumeric("2009/12") - tmp[,15]
  names(tmp) <- c(names(tmp)[1:18],"age","maxPossibleAgeInTrain") 
  
  return(tmp)
}

#returns a list of data in the same format as used in Stan model
createStanData <- function(data)
{
  moduleComponentMapper <- (unique(data[,c("moduleID","componentID","moduleComponentID")]))
  moduleComponentMapper <- moduleComponentMapper[order(moduleComponentMapper[,3]),1:2]
  
  #gives the index of previsious moduleComponentSaletimeID, 0 if it was first sell month
  tmp <- (unique(data[,c("moduleComponentID","moduleComponentSaletimeID","normalizedSaleTime")]))
  tmp <- tmp[order(tmp[,2]),]
  tmp[,4] <- tmp[,2]-1
  tmp[tmp[,3]==0,4] <- 0
  prevModuleComponentTimeMapper <- tmp[,4]
  
  stan_data <- list(
    nRows = nrow(data),
    nComponents = length(unique(data[,"componentID"])),
    nModuleComponents = length(unique(data[,"moduleComponentID"])),
    nModuleComponentTimes = length(unique(data[,"moduleComponentSaletimeID"])),
    moduleComponentMapper = moduleComponentMapper,
    prevModuleComponentTimeMapper = prevModuleComponentTimeMapper,
    component = data[,"componentID"],
    moduleComponent = data[,"moduleComponentID"],
    moduleComponentTime = data[,"moduleComponentSaletimeID"],
    repairCount = data[,"repairN"],
    saleCount = data[,"saleN"],
    age = data[,"age"],
    repairMonth = data[,'repairMonth']
  )
  return(stan_data)
}

#generate data and save to disk
generateData <- function(settings)
{
  allData <- createDataset(settings)    
  train <- allData[allData$repairYear<2010,]
  test <- allData[allData$repairYear>=2010,]
  write.csv(train, settings$CUSTOM_TRAIN_DATA_PATH, quote=FALSE, row.names=FALSE)
  write.csv(test, settings$CUSTOM_TEST_DATA_PATH, quote=FALSE, row.names=FALSE)
}

#load dataset (train or test) from disk, if missing generate.
loadData <- function(type, settings)
{
  if(type!='train' & type!='test')
  {
    cat('Wrong data type \n')
    return
  }
  
  if(!file.exists(settings$CUSTOM_TRAIN_DATA_PATH) | !file.exists(settings$CUSTOM_TEST_DATA_PATH))
  {
    cat("Data cannot be loaded from files, generating...\n")  
    generateData(settings)
  }
  
  if(type=="train")
  {
    return(read.csv(settings$CUSTOM_TRAIN_DATA_PATH, stringsAsFactors=FALSE))
  }
  
  if(type=='test')
  {
    return(read.csv(settings$CUSTOM_TEST_DATA_PATH, stringsAsFactors=FALSE))
  }
}
