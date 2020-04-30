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
                          by="Site.Code",all="TRUE", sort="FALSE")
return(CollatedData)
}

NaNdtoZero<-function(colatedData,NaNdtoZeroNames){
  #Search along each column name
  for (inames in 1:length(NaNdtoZeroNames)){
    ColumnName=NaNdtoZeroNames[inames]
    for (isamp in 1:nrow(colatedData)){
      if ((!is.numeric(colatedData[isamp,ColumnName]))||is.na(colatedData[isamp,ColumnName])){
        colatedData[isamp,ColumnName]<-0;
      }
    }
  }
  return(colatedData)
}
NormalizeData<-function(numericalData,NormalizeNames){
  for (inames in 1:length(NormalizeNames)){
    ColumnName=NormalizeNames[inames]
    #Scale
    numericalData[,ColumnName]<-scale(numericalData[,ColumnName],scale= var(numericalData[,ColumnName]))
    #Check Mean
    if (!(mean(numericalData[,ColumnName])==0)){
      stop("Mean not 0")
    }
    #Check Var
    if (!(var(numericalData[,ColumnName])==1)){
      stop("Mean not 1")
    }
  }
  
}