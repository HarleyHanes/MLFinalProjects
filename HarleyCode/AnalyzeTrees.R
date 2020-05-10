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
    dataSplit <- initial_split(data,prop=hyper_grid$sample_size[i])
    trainData <- training(dataSplit)
    testData <- testing(dataSplit)
    # train model
    if (applyWeight==TRUE){
        weights<-c(1/sum(trainData$DxPCR..Blood.==0),1/sum(trainData$DxPCR..Blood.==1))*
                  .5*(1/sum(trainData$DxPCR..Blood.==0)+1/sum(trainData$DxPCR..Blood.==1))
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