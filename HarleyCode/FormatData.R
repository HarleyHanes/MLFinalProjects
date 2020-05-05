colateData <- function(rawData,TrimNames) {
#colateData: Takes the raw data structure and combines into a 
#            single data frame with all relevant parameters
  #Trim Site Data
    siteDataTrimmed=rawData$site[ , !(names(rawData$site) %in% TrimNames$site)];
  #Trim Necropsy Data
    necropsyDataTrimmed=rawData$necropsy[ , !(names(rawData$necropsy) %in% TrimNames$necropsy)];
  #Trim Chagas Data
    chagasDataTrimmed=rawData$chagas[ , !(names(rawData$chagas) %in% TrimNames$chagas)];
  #Colate
    ##Colate necrospyData into chagasData
      ChagasNecropsy<-merge(chagasDataTrimmed,necropsyDataTrimmed,
            by="ï..RAT.ID")
    
    ##Colate siteData into chagasData
      CollatedData<-merge(ChagasNecropsy,siteDataTrimmed,
                          by="Site.Code",all="FALSE", sort="FALSE")
return(CollatedData)
}


###-----------------------------Factorize Data Function---------------------------------------
FactorizeData<-function(colatedData,FactorNames){
  FactorizedData=colatedData[ , (names(colatedData) %in% FactorNames)]
  FactorizedData[] <- lapply(FactorizedData, function(x) if(!is.factor(x)) as.factor(x) else x)
  return(FactorizedData)
}


###----------------------------Numericize Data Function---------------------------------------
NumericizeData<-function(colatedData,NumericalNames,NaNPolicy){
  NumericizedData=colatedData[,(names(colatedData) %in% NumericalNames)]
  NumericizedData[] <- lapply(NumericizedData, function(x) if(!is.numeric(x)) as.numeric(x) else x)
  NumericizedData$ï..RAT.ID=as.factor(NumericizedData$ï..RAT.ID)
  if (NaNPolicy=="Remove"){
    NumericizedData=na.omit(NumericizedData)
  } else if (NaNPolicy=='Keep'){
    
  }
  return(NumericizedData)
}




###----------------------------Normalize Data Function---------------------------------------
NormalizeData<-function(NumericalData,NormalizeNames){
  NormalizedData=NumericalData[,(names(NumericalData) %in% NormalizeNames)]
  for (inames in 2:length(NormalizeNames)){#Start at 2 so RatID isn't normalized
    ColumnName=NormalizeNames[inames]
    #Scale
    NormalizedData[,ColumnName]<-scale(NumericalData[,ColumnName],center=TRUE,scale=TRUE)
    #Convert back to numeric
    NormalizedData[,ColumnName]<-as.numeric(NormalizedData[,ColumnName])
    #Check Mean
    #if (!(mean(NumericalData[,ColumnName])==0)){
    #  warning("Mean not 0")
    #}
    #Check Var
    #if (!(var(NumericalData[,ColumnName])==1)){
    #  warning("variance not 1")
    #}
  }
  return(NormalizedData)
}

PostDataStatistics<-function(data){
  cat('\n\n----------------------DATA HEAD-----------------------\n')
  
  print(head(data))
  
  cat('\n----------------------DATA ClASSES-----------------------\n')
  print(sapply(data,class))
  
  
  cat('\n----------------------DATA SUMMARY-----------------------\n')
  
  print(summary(data))
  

  
}