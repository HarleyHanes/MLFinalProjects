#RodentForestMain
  ##main code for Random Forest analysis of Chagas Rodent data
  ##Author: Harley Hanes, 4/29/20
#Executive Control
  ##Set Directory
    rm()
    setwd("C:/Users/X1/OneDrive/Documents/Student Research/RodentChagasRisk/MLFinalProjects/HarleyCode")
  ##Datasetup
    ###What observations do we trim?
      TrimNames<-list(
          site=c("OBJECTID_12.."),
          necropsy=c(
            #Duplicate Site Information
            "Count", "Relabel", "CNH.Study Neighborhood",  "Address"
            #Remove % Coverages
            ,"X.Canopy",   "X.Grass",   "X.green"
            #Female Only Observations
            ,"Parous..females.only.",  "X..Embryos"
            #Juvenile/ Adult (Determined just from body length (Ghersi2020))
            ,"Juvenile..1.yes.0.N.","Sexually.Mature..1.yes.0.no."
            #Observations on whether Data was collected
            ,"Ectoparasites.Collected", "Urine.Collected", "Embyros.Collected..0.no..1.yes."
            ,"Liver.Cysts.Observed..0.no.1.yes.", "ddRAD.data","Capillaria.Observed"
            #Internal Necropsy with lots of missing values
            ,"Lung.Parasites", "X..Lung.Parasites", "Other.Collected","Notes","Serum"
            #Ectoparasites with lots of missing values
            ,"Bartonella.Species..from.flea.", "Sample..", "O..bacoti"
            ,"L..echidnina", "L..nuttalli", "H..glasgowi", "X.cheopis", "C..felis", "Louse.1", "Cannot.ID"
          ),
          chagas=c("Dx.PCR..Heart.")
      )
    ###What na/nd  observations to we change to 0
      #NaNdtoZeroNames=c(
        #Number of interior abnormalities
       # "Urine.Parasite","Capillaria.Collected","X..Liver.Cysts.Collected"
        #Number of exctoparasites
       # ,"Mite.Sp..notID.d.", "Tropical", "Spiny", "Unknown.Mite", "Flea..Ctenocephalides.felis"
       # ,"Xenopsylla.cheopis", "Louse", "Tick"
    #  )
    ###What do we normalize? Numerical Observations that need normalization
        #---Currently Not normalizing Probabilities---#
      NormalizeNames<-c(
        #Physiology Observations
        "Body.Length","Ear.Length", "Tail.Length", "Foot.Length", "Weight","Wound.Score"
        #Numbers of Samples
        ,"X..Blood.Samples", "X..Serum.Samples", "X..Lung.Samples", "X..Liver.Samples"
        ,"X..Spleen.Samples", "X..Kidney.Samples", "X..Tail.Samples", "Ectoparasites.Collected"
        #Numbers of Parasites Collected
        ,"Urine.Parasite","Ectoparasites.Collected"
        #Number of interior abnormalities
        ,"Urine.Parasite","Capillaria.Collected","X..Liver.Cysts.Collected"
        #Number of exctoparasites
        ,"Mite.Sp..notID.d.", "Tropical", "Spiny", "Unknown.Mite", "Flea..Ctenocephalides.felis"
        ,"Xenopsylla.cheopis", "Louse", "Tick"
        )
    ###Do we perform a PCA?
  ##Preprocessing
    ###Are we making scatters?
  
  ##Random Forest
    ###Are we training, loading, or skipping?
  
    ###Which random forests are we training/ loading?
#Load Sources
  ## User Defined Functions
      source("C:\\Users\\X1\\OneDrive\\Documents\\Student Research\\RodentChagasRisk\\Code\\Harley\\FormatData.R") #colateData, NaNdtoZero
  ## Packages

#DataSetup
  #Predefine Data Structure
  #raw<-list(site='empty',necropsy='empty',chagas='empty');
  #Data<-list(raw=raw);
  ##Load data
    ###Load Site Data
      SiteData<-read.csv("..\\..\\Data\\TrapRateLandCover.csv",stringsAsFactors=FALSE)
    ###Load Necropsy Data
      NecropsyData<-read.csv("..\\..\\Data\\NecropsyData_JAN_2019.csv",stringsAsFactors=FALSE)
    ###Load Chagas Data
      ChagasData<-read.csv("..\\..\\Data\\chagasResults.csv",stringsAsFactors=FALSE)
    ###Colate into rawData
      rawData<-list(site=SiteData,necropsy=NecropsyData,chagas=ChagasData)
    ###Remove non-list data files from workspace
      rm(SiteData,NecropsyData,ChagasData)
    
  ##Colate and Trim Data- Trim undesired predictors and combine all data tables into single dataframe
    colatedData<-colateData(rawData,TrimNames)
  ## Numericize Data- Change Values of ordinal data to numerical data
    ####--Currently changing all non-numeric or na values to 0--###
    numericalData<-NaNdtoZero(colatedData,NormalizeNames)
    head(numericalData)
  ## Normalize Data
    normalizedData<-NormalizeData(numericalData,NormalizeNames)           
  ## PCA Data
                        
#Random Forest
  ##Which