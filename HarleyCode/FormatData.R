#--------------------------------------------Load Data-----------------------------------------------
#Primary Function Call for getting data
LoadData<-function(){
  #What Observations are we trimming
    TrimNames<-list(
      site=c("OBJECTID_12.."
             #CNH Study
             ,"CNH.Study"
             #Trap 
             ,"Trap.type"
      ),
      necropsy=c(
        #Trap Data
        "Trap.type","Trap.Number","CNH.Study"
        #Duplicate Site Information
        ,"Count", "Relabel", "CNH.Study Neighborhood",  "Address"
        #Collection Dates
        ,"Day.Collected","Date.Collected"
        #Bugged Data
        ,"Ear.Length"
        #Remove % Coverages
        ,"X.Canopy",   "X.Grass",   "X.green"
        #Female Only Observations
        ,"Parous..females.only.",  "X..Embryos"
        #Juvenile/ Adult (Determined just from body length (Ghersi2020))
        ,"Juvenile..1.yes.0.N.","Sexually.Mature..1.yes.0.no."
        #Internal Samples
        ,"X..Blood.Samples",  "X..Serum.Samples",  "X..Lung.Samples",   "X..Liver.Samples",  "X..Spleen.Samples"
        ,"X..Kidney.Samples", "X..Tail.Samples", "Fecal.Oral","X..Liver.Cysts.Collected"
        #Observations on whether Data was collected
        ,"Ectoparasites.Collected", "Urine.Collected", "Embyros.Collected..0.no..1.yes."
        ,"Liver.Cysts.Observed..0.no.1.yes.", "ddRAD.data","Capillaria.Observed"
        #Internal Necropsy with lots of missing values
        ,"Lung.Parasites", "X..Lung.Parasites", "Other.Collected","Notes","Serum"
        #Ectoparasites with lots of missing values
        ,"Bartonella.Species..from.flea.", "Sample..", "O..bacoti"
        ,"L..echidnina", "L..nuttalli", "H..glasgowi", "X.cheopis", "C..felis", "Louse.1", "Cannot.ID"
        #Number of interior abnormalities
        ,"Urine.Parasite","Capillaria.Collected"
        #Number of exctoparasites
        ,"Mite.Sp..notID.d.", "Tropical", "Spiny", "Unknown.Mite", "Flea..Ctenocephalides.felis"
        ,"Xenopsylla.cheopis", "Louse", "Tick"
      ),
      chagas=c("Dx.PCR..Heart."),
      veg=c()
    )
  #What variables are we treating as factors (and what are we treating as factors and which
  # are we treating as numbers)e
    FactorNames<-c("�..RAT.ID"
                 ,"Site.Code","DxPCR..Blood.", "Neighborhood", "Season", "Alive.at.pickup"
                 ,"Species", "Sex","Alive.at.pickup.")
  #DataSetup
    ##Load data
      ###Load Site Data
        SiteData<-read.csv("..\\..\\Data\\TrapRateLandCover.csv",stringsAsFactors=FALSE)
      ###Load Necropsy Data
        NecropsyData<-read.csv("..\\..\\Data\\NecropsyData_JAN_2019Modified.csv",stringsAsFactors=FALSE)
      ###Load Chagas Data
        ChagasData<-read.csv("..\\..\\Data\\chagasResults.csv",stringsAsFactors=FALSE)
      ###Load Vegitation+ Data
        VegitationData<-LoadVegData()
      ###Colate into rawData
        rawData<-list(site=SiteData,necropsy=NecropsyData,chagas=ChagasData,veg=VegitationData)
      ###Remove non-list data files from workspace
        rm(SiteData,NecropsyData,ChagasData,VegitationData)
    
    ##Colate and Trim Data- Trim undesired predictors and combine all data tables into single dataframe
      colatedData<-colateData(rawData,TrimNames)
      rm(rawData)
    ##Format Data- Set all observations to class or factors
    ## Factorize Data
      FactorizedData<-FactorizeData(colatedData, FactorNames)
    ## Numericize Data- Change Values of ordinal data to numerical data
      ####--Currently changing all non-numeric or na values to 0--###
    
      NumericalNames<-c("�..RAT.ID", names(colatedData)[!(names(colatedData) %in% FactorNames)])
      NumericizedData<-NumericizeData(colatedData,NumericalNames,"Remove")
      NormalizedData<-NormalizeData(NumericizedData,NumericalNames) 
    ##Combine factor and numericalData
      CleanedData<-merge(FactorizedData,NormalizedData,
                       by="�..RAT.ID",all="FALSE", sort="FALSE")
    ## Trim site code and Rat ID
      CleanedData<-CleanedData[,!(names(CleanedData) %in% c("�..RAT.ID","Site.Code"))]
    ##Post statistics
      PostDataStatistics(CleanedData)
      print('Colated Data Dimensions:')
      cat(dim(colatedData))
      print('Cleaned Data Dimensions:')
      cat(dim(CleanedData))
      rm(colatedData,NormalizedData,NumericizedData,FactorizedData)
      #dataResults<-list('data'=CleanedData)
    ##PCA Data
      #if (applyPCA==TRUE)
  #return(dataResults)
    return(CleanedData)
}





#--------------------------------------------Colate Data---------------------------------------------
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
            by="�..RAT.ID")
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
  NumericizedData$�..RAT.ID=as.factor(NumericizedData$�..RAT.ID)
  #NumericizedData$Site.Code=as.factor(colatedData$Site.Code)
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

#----------------------------------Run PCA------------------------------------r
runPCA<-function(data){
  #Transform factors to numerics
  numData<-data
  numData[,sapply(data,class)=="factor"]<-lapply(data[,sapply(data,class)=="factor"],as.numeric)
  #Get PCA data
  pcaData<-prcomp(numData[,!(names(numData) %in% c('DxPCR..Blood.'))], center=TRUE, scale.=TRUE)
  
  #save to PCAresult list
  
  return(pcaData)
  
  
}