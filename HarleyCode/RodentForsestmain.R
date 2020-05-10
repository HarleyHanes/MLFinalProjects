#-----------------------------------------------RodentForestMain.R----------------------------------------------
  ##main code for Random Forest analysis of Chagas Rodent data
  ##Author: Harley Hanes, 2020
  ##Data Credit:
  ##Github Repo: https://github.com/HarleyHanes/MLFinalProjects
  ##Note: Code requires access to datasets not included in git repository
#-----------------------------------------------Executive Control------------------------------------------------
  ##-------------------------------Set Directory
  rm(list=ls())
    setwd("C:/Users/X1/OneDrive/Documents/Student Research/RodentChagasRisk/MLFinalProjects/HarleyCode")
  ##-------------------------------Data Setup
    ###Do we perform a PCA?
      applyPCA=TRUE
      applyWeights=TRUE
  ##-------------------------------Random Forest
    ### What model are we running?
      modelType<-'Dimension Reduction'
      if (modelType=='Tune Forest'){
        ###Tuning
        importanceType='impurity'#How do we measure importance at node splits- permutation or impurity
        errortype<-c('specificity','sensitivity','totalError') #What are our metrics of error (must be included)
      
      }else if(modelType=='Dimension Reduction'){
        ###Optimal PCA
        importanceType='impurity'#How do we measure importance at node splits- permutation or impurity
        errortype<-c('specificity','sensitivity','totalError') #What are our metrics of error (must be included)
        reductionStep<-.8 #What upper proportion of observations do we keep (rounding up)
        reductionError<-'totalError'#What error value do we use to select optimal model
      }
#-----------------------------------------------Load Functions and Packages--------------------------------------------
  ##-----------------------User Defined Functions
      source("FormatData.R") #colateData, NaNdtoZero
      source("AnalyzeTrees.R")
  ##-----------------------Packages
      library(rsample) # data splitting
      #library(randomForest) # basic implementation
      library(ranger) # a faster implementation of r
      #library(e1071) #Used for confusion matrix
      library(caret) # an aggregator package for pe
      library(dplyr)
      library(broom)
      #library(h2o) # an extremely fast java-based
#-----------------------------------------------Load and Format Data-------------------------------------
    ## Load Data
      data=LoadData()
    ##PCA Data
      if (applyPCA==TRUE){
        PCAresult<-runPCA(data)
        PCAdata<-as.data.frame(PCAresult$x)
        PCAdata$DxPCR..Blood.<-data$DxPCR..Blood.
      }
      
#-----------------------------------------------Tune Forest---------------------------------
if (modelType=='Tune Forest'){
    #Find optimal settings
      #applyWeights=TRUE
      if (applyPCA==TRUE){
        #Tune
        hyperGrid<-SearchHyperParameters(PCAdata,errortype,applyWeights,importanceType)
        #hyperGrid<-SearchHyperParameters(PCAdata[,(names(PCAdata) %in% c('DxPCR..Blood.',topNames))],errortype,applyWeights)
        #Get Optimal Model
        optimalModel<-hyperGrid[which.max(hyperGrid$sensitivity),]
        #Get Impurity plot
        GetImpurity(PCAdata,optimalModel,importanceType)
      } else {
        hyperGrid<-SearchHyperParameters(data,errortype,applyWeights,importanceType)
      }
        
    #Make Scatter plot of Sensitivity and Specificity
      plot(hyperGrid$sensitivity[-which.max(hyperGrid$sensitivity)],
           hyperGrid$specificity[-which.max(hyperGrid$sensitivity)],
           pch=16,
           col='black',
           ylim=c(min(hyperGrid$specificity), max(hyperGrid$specificity)),
           xlim=c(min(hyperGrid$sensitivity), max(hyperGrid$sensitivity)),
           xlab='Sensitivity',
           ylab='Specificity')
        points(hyperGrid$sensitivity[which.max(hyperGrid$sensitivity)],
             hyperGrid$specificity[which.max(hyperGrid$sensitivity)],
             col='blue',
             pch=16)
        points(hyperGrid$sensitivity[which.min(hyperGrid$totalError)],
               hyperGrid$specificity[which.min(hyperGrid$totalError)],
               col='red',
               pch=16)
        
        #legend(legend=c("Sample Points", "Maximum Sensitivity","Minimum Error"),
         #      col=c("black","red", "blue"), lty=1:2, cex=0.8,title="Point Types")
    #Correlation Matrix 
      #var(scale(hyperGrid))
    #print(parameterSearch$bestParameter$error)
} 
#-----------------------------------------------Dimension Reduction-------------------------------
if (modelType== 'Dimension Reduction'){
  ##--------------------------------Get Reduced Models
    ###Setup models dataframe
    modelsNames<-c("mtry", "node_size", "sample_size",errortype, "num_Predictors")
    models<-data.frame(matrix(ncol=length(modelsNames),nrow=0))
    names(models)<-modelsNames
    ###Setup Predictors dataframe
    ### Setup dataReduced
    if (applyPCA==TRUE){
      dataReduced<-PCAdata
      predictors<-list(names(PCAdata[,!(names(PCAdata) %in% c("DxPCR..Blood."))]))
    } else {
      dataReduced<-data
      #predictors<-ls(names(data[,!(names(data) %in% "DxPCR..Blood.")]))
    }
    #Get number of predictors
    pPredictors<-ncol(dataReduced)-1
  while (pPredictors>1){
  ##-------------------------------Get Optimal Model
    ###Get Hyper Grid
    hyperGrid<-SearchHyperParameters(dataReduced,errortype,applyWeights,importanceType)
    ###Extract Optimum
    if (reductionError=='totalError'){
      optimalModel<-hyperGrid[which.min(hyperGrid[,names(hyperGrid) %in% reductionError]),]
    } else{
      optimalModel<-hyperGrid[which.max(hyperGrid[,names(hyperGrid) %in% reductionError]),]
    }
    optimalModel$num_Predictors<-pPredictors
  ##-------------------------------Record Model Statistics
    ###Record Hyper_grid parameter Settings and error
    models<-rbind(models,optimalModel)
  ##-------------------------------Get New predictors
    ###Get ranked old predictors
    rankedPredictors<-GetImpurity(dataReduced,optimalModel,importanceType,plotBool<-FALSE)
    ###Find how many Names to get
    numNames<-ceiling(length(rankedPredictors)*reductionStep)
    #numNames<-length(rankedPredictors)-4
    ## Make sure the number of new predictors is at least 
    if (pPredictors==numNames){
      numNames<-numNames-1
    }
    topNames<-names(rankedPredictors[1:numNames])
    #Save topNames
    predictors[[length(predictors)+1]]<-topNames
  ##-------------------------------Remove Observations
  dataReduced<-dataReduced[,names(dataReduced) %in% c('DxPCR..Blood.',topNames)]
  pPredictors<-numNames
  print(pPredictors)
  #rm(rankedPredictors,numNames,optimalModel,hyperGrid,topNames)
  }
  ##------------------------------Plot Errors vs. # Principal Components
      
    ggplot(models)+
      geom_line(aes(x=num_Predictors,y=totalError*100,color="red"))+
      geom_line(aes(x=num_Predictors,y=sensitivity*100,color="blue"))+
      geom_line(aes(x=num_Predictors,y=specificity*100,color="green"))+
      labs(x='Number of Predictors',y='%')+
      #scale_color_discrete(name="Error Type",values=c("Total Error"='red',"Sensitivity"='blue',"Specificity"='green'))+
      scale_color_discrete(name="Error Measure",labels=c("Sensitivity","Specificity","Total Error"))+
      theme(legend.position = c(0.9, 0.65))+
      coord_cartesian(xlim = c(2, 66), ylim = c(0, 100))
  
  ##------------------------------Get Biplot
}        


#--------------------------------------------Deprecated Code------------------------------------------------
      #Random Forest
      ##Split
      dataSplit <- initial_split(PCAdata,prop=.7)
      trainData <- training(dataSplit)
      testData <- testing(dataSplit)
      dim(trainData)
      dim(testData)
      splitData<-list(testData,trainData)
      names(splitData)<-c('test','train')
      treeSettings<-list(minNum=1,maxNum=1100)
      results<-PlotTreeNumber(trainData,testData,treeSettings) 
      ggplot(results)+
        geom_line(aes(x=NumTrees,y=Sensitivity,color='blue'))+
        geom_line(aes(x=NumTrees,y=Specificity,color='green'))+
        theme(legend.position = c(0.9, 0.5))+
        labs(x='Number of Trees',y="%")+
        scale_color_discrete(name = "Error Type", labels = c("Sensitivity", "Specificity"))
      ##Train a classification model
      #Trainedmodel<-ranger(
      # formula = DxPCR..Blood. ~ .,
      # data=splitData$train,
      #  num.trees=500,
      #  mtry=5,
      #  write.forest=TRUE,
      #treetype='classification', #'regression'
      #  min.node.size=5,
      #  sample.fraction=.8,
      #  importance='impurity'
      # )
      
      ##Test prediction
      #testResult<-predict(Trainedmodel,
      #                      splitData$test,
      #                      type='response')
      #r<-confusionMatrix(testResult$predictions,testData$DxPCR..Blood.)
      #treeSettings<-data.frame("minDepth"=1,"maxDepth"=151)
      #results<-PlotTreeDepth(splitData$train,treeSettings)
      #plot(results$vecDepth,results$Sensitivity, type="l")
      #ggplot(results, aes(vecDepth,Specificity))
      #plot(results$vecDepth,results$Specificity, type="l")
      