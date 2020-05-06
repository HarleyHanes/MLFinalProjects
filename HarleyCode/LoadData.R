LoadVegData<-function{
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
}
#Clear Datasets with missing values
AverageVegData<-AverageVegData[ , colSums(is.na(AverageVegData)) == 0]
}