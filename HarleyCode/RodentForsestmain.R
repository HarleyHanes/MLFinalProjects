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
      modelType<-'Tuning'
      if (modelType=='Tuning'){
        ###Tuning
        importanceType='permutation'#How do we measure importance at node splits- permutation or impurity
        errortype<-c('specificity','sensitivity','totalError') #What are our metrics of error (must be included)
        
      }else if(modelType=='Optimal PCA'){
        ###Optimal PCA
        importanceType='permutation'#How do we measure importance at node splits- permutation or impurity
        errortype<-c('specificity','sensitivity','totalError') #What are our metrics of error (must be included)
      }
#------------------------------------------Load Functions and Packages--------------------------------------------
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
#---------------------------------------------------Load and Format Data-------------------------------------
    ## Load Data
      data=LoadData()
    ##PCA Data
      if (applyPCA==TRUE){
        PCAresult<-runPCA(data)
        PCAdata<-as.data.frame(PCAresult$x)
        PCAdata$DxPCR..Blood.<-data$DxPCR..Blood.
      }
#------------------------------------------------------Tune Forest
    
    
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
      var(scale(hyperGrid))
    #print(parameterSearch$bestParameter$error)

#--------------------------------------------Deprecated Code------------------------------------------------
      #Random Forest
      ##Split
      #dataSplit <- initial_split(data,prop=.7)
      #trainData <- training(dataSplit)
      #testData <- testing(dataSplit)
      #dim(trainData)
      #dim(testData)
      #splitData<-list(testData,trainData)
      #names(splitData)<-c('test','train')
      
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
      