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