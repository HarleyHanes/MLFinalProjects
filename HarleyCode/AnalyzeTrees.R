PlotTreeDepth<-function(trainData,testData,treeSettings){
  
  
  vecDepth=seq(treeSettings$minDepth, treeSettings$maxDepth, by=5)
  Sensitivity<-c()
  Specificity<-c()
  Error<-c()
  for (i in 1:length(vecDepth)){
    #Make Forest
    Trainedmodel<-ranger(
      formula = DxPCR..Blood. ~ .,
      data=trainData,
      num.trees=500,
      mtry=5,
      max.depth=vecDepth[i],
      #treetype='classification', #'regression'
      min.node.size=5,
      sample.fraction=.8,
      importance='impurity'
    )
    testResult<-predict(Trainedmodel,
                        testData,
                        type='response')
    
    Confusion<-confusionMatrix(testResult$predictions,testData$DxPCR..Blood.)
    #Error[i]<-sum(!testResult$predictions==testData$DxPCR..Blood.)/norw(testResult)
    Sensitivity[i]<-Confusion$byClass[1]
    Specificity[i]<-Confusion$byClass[2]
  }
  #results<-data.frame('vecDepth'<-vecDepth,'Sensitivity'<-Sensitivity, 'Specificity'<-Specificity)
  results<-data.frame(vecDepth,Sensitivity,Specificity)
 return(results)
}

PlotTreeNumber<-function(trainData,testData,treeSettings){
  
  
  NumTrees=round(seq(treeSettings$minNum, treeSettings$maxNum, length=30))
  Sensitivity<-c()
  Specificity<-c()
  Error<-c()
  for (i in 1:length(NumTrees)){
    #Make Forest
    Trainedmodel<-ranger(
      formula = DxPCR..Blood. ~ .,
      data=trainData,
      num.trees=NumTrees[i],
      mtry=5,
      #treetype='classification', #'regression'
      min.node.size=5,
      sample.fraction=.8,
      importance='impurity'
    )
    testResult<-predict(Trainedmodel,
                        testData,
                        type='response')
    
    Confusion<-confusionMatrix(testResult$predictions,testData$DxPCR..Blood.)
    #Error[i]<-sum(!testResult$predictions==testData$DxPCR..Blood.)/norw(testResult)
    Sensitivity[i]<-Confusion$byClass[1]
    Specificity[i]<-Confusion$byClass[2]
  }
  #results<-data.frame('vecDepth'<-vecDepth,'Sensitivity'<-Sensitivity, 'Specificity'<-Specificity)
  results<-data.frame(NumTrees,Sensitivity,Specificity)
  return(results)
}

SearchHyperParameters<-function(data,errortype,applyWeight,importanceType){
  
  #Define hypergrid to search on
  hyper_grid <- expand.grid(
    mtry = unique(round((length(names(data))-1)*seq(.2, .8, length= 10))),
    node_size = unique(round(seq(1, .5*sqrt((nrow(data))), length=10))),
    sample_size = c(.5, .6, .7, .8)
  )
  
  #Apply model on hypergrid
  for(i in 1:nrow(hyper_grid)) {
    #Split data
    dataSplit <- GetDataSplit(data,hyper_grid$sample_size[i])
    trainData <- dataSplit$train
    testData <- dataSplit$test
    # train model
    if (applyWeight==TRUE){
        weights<-c(sum(trainData$DxPCR..Blood.==1),sum(trainData$DxPCR..Blood.==0))
        weights<-weights/mean(weights)
        #weights<-c(1/sum(trainData$DxPCR..Blood.==0),1/sum(trainData$DxPCR..Blood.==1))*
        #          .5*(1/sum(trainData$DxPCR..Blood.==0)+1/sum(trainData$DxPCR..Blood.==1))
      model <- ranger(
        formula = DxPCR..Blood. ~.,
        data = trainData,
        num.trees = 500,
        mtry = hyper_grid$mtry[i],
        min.node.size = hyper_grid$node_size[i],
        seed = 123,
        importance=importanceType,
        class.weights=weights
      )
    } else {
      model <- ranger(
        formula = DxPCR..Blood. ~.,
        data = trainData,
        num.trees = 500,
        mtry = hyper_grid$mtry[i],
        min.node.size = hyper_grid$node_size[i],
        seed = 123,
        importance=importanceType
      )
      
    }
    #Test model
    p<-predict(model,
                testData,
                type='response')
    # add OOB error to grid
    for (j in 1:length(errortype)){
      hyper_grid[i,3+j] <- GetError(errortype[j],p$predictions,testData$DxPCR..Blood.)
    }
  }
  names(hyper_grid)<-c(names(hyper_grid[,1:3]),errortype)
  #bestParameter<-hyper_grid[,which.min(hyper_grid$error)]
  #OptimalResults<-list(hyper_grid,bestParameter)
  return(hyper_grid)
  
}

GetError<-function(errortype,prediction,true){
  Confusion<-confusionMatrix(prediction,true)
    if (errortype=="specificity"){
      #Error[i]<-sum(!testResult$predictions==testData$DxPCR..Blood.)/norw(testResult)
      error<-Confusion$byClass[1]
    } else if (errortype=='sensitivity'){
      error<-Confusion$byClass[2]
    } else if (errortype=='totalError'){
      error<-sum(!(prediction==true))/length(true)
    }
  
}

GetImpurity<-function(data,optimalModel,importanceType,plotBool){
  #Split
  dataSplit <- initial_split(data,prop=optimalModel$sample_size)
  trainData <- training(dataSplit)
  testData <- testing(dataSplit)
  dim(trainData)
  dim(testData)
  splitData<-list(testData,trainData)
  names(splitData)<-c('test','train')
  #Train
  Trainedmodel<-ranger(
    formula = DxPCR..Blood. ~ .,
    data=splitData$train,
    num.trees=500,
    mtry=optimalModel$mtry,
    write.forest=FALSE,
    #treetype='classification', #'regression'
    min.node.size=optimalModel$node_size,
    importance= importanceType
  )
  if (plotBool==TRUE){
    #Plot Rankings
    Trainedmodel$variable.importance %>%
      tidy() %>%
      dplyr::arrange(desc(x)) %>%
      dplyr::top_n(15) %>%
      ggplot(aes(reorder(names, x), x)) +
      geom_col() +
      coord_flip()+
      labs(y=paste('Decrease in ',importanceType),
           x='Predictor') +
      ggtitle("Top 15 Important Predictors")
  }
  
  
  return(sort(Trainedmodel$variable.importance, decreasing=TRUE))
  
}

GetReductionHist<-function(P){
  #Get Predictor Names[]
  PredNames<-names(P)
  #Define empty PredFrame- make nx0 and add each predNames as a new column
  predFrame<-data.frame(id=1:length(unlist(P[1],use.name=FALSE)))
  #Unlist predictors
  for (i in 1:length(PredNames)){
    #predFrame[,PredNames[i]]<-unlist(P[PredNames[i]],use.name=FALSE)
    predFrame[,i]<-unlist(P[PredNames[i]],use.name=FALSE)
  }
  # Correct Names
  names(predFrame)<-PredNames
  #turn into factor to get ordering
  predTestFactor<-as.factor(predFrame[,1])
  #Get 1,2,3,4... ordering
  ordering<-unique(predTestFactor)
  #Make levels correct !!!This only works if the first names are ordered correctly!!!!
  for (i in 1:ncol(predFrame)){
    predFrame[,i]<-factor(predFrame[,i],levels=unique(predTestFactor))
  }
  
  #Make histogram
  
  ggplot(predFrame) + 
    geom_bar(aes(x=predFrame[,2],fill='Weighted, Impurity, Sensitivity'), stat='count')+
    geom_bar(aes(x=predFrame[,4],fill='Weighted, Permutation, Sensitivity'), stat='count')+
    geom_bar(aes(x=predFrame[,3],fill='Weighted, Impurity, Total Error'), stat='count')+
    geom_bar(aes(x=predFrame[,1],fill='Unweighted, Impurity, Total Error'), stat='count')+
    labs(fill='Dimension Reduction Simulation')+
    xlab('Principal Component')+
    ylab('Dimension Reduction Iteration Removed')+
    scale_x_discrete(labels = c("PC1"," ", " ", " ",  
                               "PC5"," ", " ", " ", " ", 
                               "PC10"," ", " ", " ", " ", 
                               "PC15"," ", " ", " ", " ", 
                               "PC20", " ", " ", " ", " ",
                               "PC25"," ", " ", " ", " ", 
                               "PC30"," ", " ", " ", " ", 
                               "PC35"," ", " ", " ", " ", 
                               "PC40"," ", " ", " ", " ", 
                               "PC45", " ", " ", " ", " ",
                               "PC50"," ", " ", " ", " ", 
                               "PC55"," ", " ", " ", " ", 
                               "PC60"," ", " ", " ", " ", 
                               "PC65"))
}

PCtoObserveImportanceHist<-function(PCAdata,PCAresult,optimalModel,importanceType,PCAnames,numbers){
  
  ReducedData<-PCAdata[,names(PCAdata) %in% c("DxPCR..Blood.",PCAnames)]
  rotation<-PCAresult$rotation[,numbers]
  
  PredVec<-GetPredictorImportance(ReducedData,optimalModel,rotation)
  
  PredVec %>%
    tidy() %>%
    dplyr::arrange(desc(abs(x))) %>%
    dplyr::top_n(15) %>%
    ggplot(aes(reorder(names, x), x)) +
    geom_col() +
    coord_flip()+
    labs(y=paste('Decrease in ',importanceType),
         x='Predictor') +
    ggtitle("Top 15 Important Predictors")
}

GetDataSplit<-function(data,pSplit){
  #Seperate Data into positives and negatives
  dataPos=data[data$DxPCR..Blood.==1,]
  dataNeg=data[data$DxPCR..Blood.==0,]
  
  #Split positives
  posSplit <- initial_split(dataPos,pSplit)
  trainData <- training(posSplit)
  testData <- testing(posSplit)
  splitPosData<-list(testData,trainData)
  names(splitPosData)<-c('test','train')
  #Split negatives
  negSplit <- initial_split(dataNeg,pSplit)
  trainData <- training(negSplit)
  testData <- testing(negSplit)
  splitNegData<-list(testData,trainData)
  names(splitNegData)<-c('test','train')
  
  splitData<-splitPosData
  #Recombine into Test
  splitData$test<-rbind(splitPosData$test,splitNegData$test)
  
  #Recombine into Train
  splitData$train<-rbind(splitPosData$train,splitNegData$train)
  
  return(splitData)
}

MakeBiplot<-function(data,PCAresults,optimalModel,importanceType,PCAnames,numbers){
  #Hyper-Parameters,
    dataNum=300 #Plot every 1 in ___ data points
    predNum=5 #Plot the ____ most important predictors
  #Extract principal components
    ##from data
      ReducedData<-PCAdata[,names(PCAdata) %in% c("DxPCR..Blood.",PCAnames)]
    ##from rotation
      rotation<-PCAresult$rotation[,numbers]
  #Get Predictor Vectors
    ##Find Predictor Importance
      PredImportances<-GetPredictorImportance(ReducedData,optimalModel,rotation)
    ##Get indices of top 15
      index <- PredImportances>= sort(PredImportances, decreasing=T)[predNum]
      ###IS IT APPROPRIATE TO TAKE THE ABSOLUTE VALUES
    ##Scale vector direction by importance
      PredVectors<-PredImportances*rotation

  
  #Make Plot
    biplot(ReducedData[floor(seq(1,nrow(ReducedData),length=dataNum)),!(names(ReducedData) %in%c("DxPCR..Blood."))],
           PredVectors[index,],
           var.axes=TRUE,
           c('green','red'),
           cex = rep(par("cex"), 2),
           xlab=PCAnames[1],
           ylab=PCAnames[2])
  #Make plot of data points
    ##Plot every tenth point
      ###Red Squares for positives, Green Circles for negatives
    
  
    ##Add legend
    
  #Add vectors of most important parameters
    ##Take 10 most important observed predictors
  
    ##Translate to vectors in PC space
      ###Define vector length by importance
  
      ###Define vector length by variance
  
    
  
}

GetPredictorImportance<-function(data,optimalModel,rotation){
  #Get importance rankings
  #Split
  dataSplit <- initial_split(data,prop=optimalModel$sample_size)
  trainData <- training(dataSplit)
  testData <- testing(dataSplit)
  splitData<-list(testData,trainData)
  names(splitData)<-c('test','train')
  #Train
  Trainedmodel<-ranger(
    formula = DxPCR..Blood. ~ .,
    data=splitData$train,
    num.trees=500,
    mtry=optimalModel$mtry,
    write.forest=FALSE,
    #treetype='classification', #'regression'
    min.node.size=optimalModel$node_size,
    importance= importanceType
  )
  PredictorImportance<-t(Trainedmodel$variable.importance) %*% t(rotation)
  observationNames<-attr(PredictorImportance,"dimnames")[[2]]
  PredVec<-PredictorImportance[1:65]
  names(PredVec)<-observationNames
  return(PredVec)
}

