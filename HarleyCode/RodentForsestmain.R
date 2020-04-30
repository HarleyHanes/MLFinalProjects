#RodentForestMain
  ##main code for Random Forest analysis of Chagas Rodent data
  ##Author: Harley Hanes, 4/29/20
#Executive Control
  ##Datasetup
    ###What observations do we trim?
      TrimNames<-list(
          site=c("OBJECTID_12.."),
          necropsy=c(
            #Duplicate Site Information
            "Count", "Relabel", "CNH.Study Neighborhood",  "Address"
            #Female Only Observations
            ,"Parous..females.only."
            #Internal Necropsy with lots of missing values
            ,"Lung.Parasites", "X..Lung.Parasites", "Other.Collected","Notes","Serum"
            #Ectoparasites with lots of missing values
            ,"Bartonella.Species..from.flea.", "Sample..", "O..bacoti"
            ,"L..echidnina", "L..nuttalli", "H..glasgowi", "X.cheopis", "C..felis", "Louse.1", "Cannot.ID"
          ),
          chagas=c("Dx.PCR..Heart.")
      )
    ###Do we normalize the data?
  
    ###Do we perform a PCA?
  ##Preprocessing
    ###Are we making scatters?
  
  ##Random Forest
    ###Are we training, loading, or skipping?
  
    ###Which random forests are we training/ loading?
#Load Sources
  ## User Defined Functions
      source("colateData.R")
  ## Packages

#DataSetup
  #Predefine Data Structure
  #raw<-list(site='empty',necropsy='empty',chagas='empty');
  #Data<-list(raw=raw);
  ##Load data
    ###Load Site Data
      SiteData<-read.csv("C:\\Users\\X1\\OneDrive\\Documents\\StudentResearch\\RodentChagasRisk\\Data\\TrapRateLandCover.csv")
    ###Load Necropsy Data
      NecropsyData<-read.csv("C:\\Users\\X1\\OneDrive\\Documents\\StudentResearch\\RodentChagasRisk\\Data\\RodentClassification\\Data\\NecropsyData_JAN_2019.csv")
    ###Load Chagas Data
      ChagasData<-read.csv("C:\\Users\\X1\\OneDrive\\Documents\\StudentResearch\\RodentChagasRisk\\Data\\RodentClassification\\Data\\chagasResults.csv")
    ###Colate into rawData
      rawData<-list(site=SiteData,necropsy=NecropsyData,chagas=ChagasData)
    ###Remove non-list data files from workspace
      rm(SiteData,NecropsyData,ChagasData)
    
  ##Colate and Trim Data- Trim undesired predictors and combine all data tables into single dataframe
    colatedData<-colateData(rawData,TrimNames)
  ## Numericize Data- Change Values of ordinal data to numerical data
    ####--Currently these values are not being analyzed--###
  ## Normalize Data
                        
  ## PCA Data
                        
#Random Forest
  ##Which