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

NaNdtoZero<-function(colatedData,NumericalNames){
  #intitialize numericalData
    CatNames<-c("ï..RAT.ID",NumericalNames)
    numericalData<-data.frame(matrix(NA,ncol=length(CatNames),nrow=length(colatedData[,1])))
    colnames(numericalData)<-CatNames;
    
  #Isolate and numericize numeric entries
    for (inames in 1:length(CatNames)){
      ColumnName=CatNames[inames]
      numericalData[,ColumnName]=as.numeric(colatedData[,ColumnName])
    }
  #Assign 0 to NA values (all values that were NA from collating or non-numeric in excel table)
  #for (isamp in 1:nrow(colatedData)){
  # if (is.na(colatedData[isamp,ColumnName])){
  #    colatedData[isamp,ColumnName]<-0;
  #  }
  #merge colatedData into numerical Data
    numericalData<-merge(colatedData,numericalData,
                          by="ï..RAT.ID",all.y=TRUE)
  return(numericalData)
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