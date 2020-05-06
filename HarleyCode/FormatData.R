colateData <- function(rawData,TrimNames) {
#colateData: Takes the raw data structure and combines into a 
#            single data frame with all relevant parameters
  #Trim Site Data
    siteDataTrimmed=rawData$site[ , !(names(rawData$site) %in% TrimNames$site)];
  #Trim Necropsy Data
    necropsyDataTrimmed=rawData$necropsy[ , !(names(rawData$necropsy) %in% TrimNames$necropsy)];
  #Trim Chagas Data
    chagasDataTrimmed=rawData$chagas[ , !(names(rawData$chagas) %in% TrimNames$chagas)];
  #Trim Chagas Data
    vegDataTrimmed=rawData$veg[ , !(names(rawData$veg) %in% TrimNames$veg)];
  #Colate
    ##Colate necrospyData into chagasData
      ChagasNecropsy<-merge(chagasDataTrimmed,necropsyDataTrimmed,
            by="ï..RAT.ID")
    ##Colate site and VegData
      VegSite<-merge(vegDataTrimmed,siteDataTrimmed[, c("Site.Code", setdiff(names(siteDataTrimmed),names(vegDataTrimmed)))], by="Site.Code", all=FALSE)
    
    ##Colate siteData into chagasData
      CollatedData<-merge(ChagasNecropsy,VegSite[, c("Site.Code", setdiff(names(VegSite),names(ChagasNecropsy)))],
                          by="Site.Code",all=FALSE, sort=FALSE)
return(CollatedData)
}


#-------------------------------------Get and Format Vegitation Data-------------------------------
LoadVegData<-function(){
  rm(list=ls())
  setwd("C:/Users/X1/OneDrive/Documents/Student Research/RodentChagasRisk/MLFinalProjects/HarleyCode")
  VegData<-read.csv("..\\..\\Data\\EnvironmentalVariablesTrimmed.csv",stringsAsFactors=FALSE)
  #Remove Year and Season
  VegData<-VegData[,-c(2,3,4)]
  FactorCols=1:2;
  NumCols=3:ncol(VegData)
  #Change to numerics and factors
  VegData[,FactorCols]<-lapply(VegData[,FactorCols],factor)
  VegData[,NumCols]<-lapply(VegData[,NumCols],as.numeric)
  #Get a Vector of all Site Codes
  SiteCodes=levels(VegData[,1])
  #For loop through all Site Codes
  for (isite in 1:length(SiteCodes)){
    #Identify site
    site=SiteCodes[isite]
    #Prep dataframes used in for loop
    siteObserved=FALSE
    #For loop through all observations to record all observations for a given site
    for (iobs in 1:nrow(VegData)){
      if (site==VegData[iobs,1]){
        #Record VegData Obesrvation
        if (siteObserved==FALSE){
          SiteData<-VegData[iobs,]
        }else{
          rbind(SiteData,VegData[iobs,])
        }
        #Remove observation from Vegdata to speed up for looping
        #VegData<-VegData[-iobs,]
        siteObserved==TRUE #Assume observations are sorted by site.
      } else if (siteObserved==TRUE){
        #break after first observation that doesn't match
        #break
      }
    }
    #Average numerical observations
    AverageSite<-SiteData[1,]
    AverageSite[,NumCols]<-colMeans(SiteData[,NumCols])
    if (isite==1){
      AverageVegData<-AverageSite
    } else{
      AverageVegData[isite,]<-AverageSite
    }
  }
  #Clear Datasets with missing values
  AverageVegData<-AverageVegData[ , colSums(is.na(AverageVegData)) == 0]
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
    NumericizedData2=na.omit(NumericizedData)
    #print(nrow(NumericizedData)-nrow(NumericizedData2), " observations were removed")
  } else if (NaNPolicy=='Keep'){
    
  }
  return(NumericizedData2)
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