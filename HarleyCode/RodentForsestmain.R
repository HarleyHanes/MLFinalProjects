#RodentForestMain
  ##main code for Random Forest analysis of Chagas Rodent data
  ##Author: Harley Hanes, 4/29/20
#Executive Control
  ##Set Directory
  rm(list=ls())
    setwd("C:/Users/X1/OneDrive/Documents/Student Research/RodentChagasRisk/MLFinalProjects/HarleyCode")
  ##Data Setup
    ###Do we perform a PCA?
  ##Preprocessing
    ###Are we making scatters?
  
  ##Random Forest
    ###Are we training, loading, or skipping?
  
    ###Which random forests are we training/ loading?
#Load Sources
  ## User Defined Functions
      source("FormatData.R") #colateData, NaNdtoZero
      source("AnalyzeTrees.R")
      library(rsample) # data splitting
      #library(randomForest) # basic implementation
      library(ranger) # a faster implementation of r
      #library(e1071) #Used for confusion matrix
      library(caret) # an aggregator package for pe
      #library(dplyr)
      #library(broom)
      #library(h2o) # an extremely fast java-based
  ## Packages
#Format Data Data
    ## Load Data
      data=LoadData()
    ##PCA Data
      #if (PCA==TRUE){
      #  PCAresults=runPCA(data)
      #  data=PCAresults$data
      #}
    
    
#Random Forest
  ##Split
    dataSplit <- initial_split(data,prop=.7)
    trainData <- training(dataSplit)
    testData <- testing(dataSplit)
    dim(trainData)
    dim(testData)
      
  ##Train a classification model
    Trainedmodel<-ranger(
      formula = DxPCR..Blood. ~ .,
      data=trainData,
      num.trees=500,
      mtry=5,
      write.forest=TRUE,
      #treetype='classification', #'regression'
      min.node.size=5,
      sample.fraction=.8,
      importance='impurity'
    )
      
  ##Test prediction
    testResult<-predict(Trainedmodel,
                          testData,
                          type='response')
    r<-confusionMatrix(testResult$predictions,testData$DxPCR..Blood.)
    treeSettings<-data.frame("minDepth"=1,"maxDepth"=151)
    results<-PlotTreeDepth(trainData,testData,treeSettings)
    #plot(results$vecDepth,results$Sensitivity, type="l")
    ggplot(results, aes(vecDepth,Specificity))
    plot(results$vecDepth,results$Specificity, type="l")
      #Trainedmodel$variable.importance %>%
      #  tidy() %>%
      #  dplyr::arrange(desc(x)) %>%
      #  dplyr::top_n(25) %>%
      #  ggplot(aes(reorder(names, x), x)) +
      #  geom_col() +
      #  coord_flip() +
      #  ggtitle("Top 25 important variables")

      