source("AnalyzeTrees.R")
optimalModel<-models[models$num_Predictors==2,]
PCAnames<-predictors[[18]]
numbers<-c(4,33)
PCtoObserveImportanceHist(PCAdata,PCAresult,optimalModel,importanceType,PCAnames,numbers)
MakeBiplot(PCAdata,PCAresult,optimalModel,importanceType,PCAnames,numbers)
































#numData<-data
#numData[,sapply(data,class)=="factor"]<-lapply(data[,sapply(data,class)=="factor"],as.numeric)
#PCAdata[1,1:ncol(PCAdata)-1]-numData[1,!(names(numData) %in% c("DxPCR..Blood."))]*PCAresult$rotation

t(t(as.matrix(PCAdata[,!(names(PCAdata) %in% c("DxPCR..Blood."))]) %*% t(PCAresult$rotation)) * PCAresult$scale + PCAresult$center)-numData[,!(names(numData) %in% c("DxPCR..Blood."))]


#Let t be a vector of importances
#as.matrix(PCAdata[,!(names(PCAdata) %in% c("DxPCR..Blood."))])

PCAnames<-predictors[[18]]
optimalModel<-models[18,]
numbers<-c(33,50)

PCAdata<-PCAdata[,names(PCAdata) %in% c("DxPCR..Blood.",PCAnames)]
#Get importance rankings
#Split
dataSplit <- initial_split(PCAdata,prop=optimalModel$sample_size)
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
#Transform PCAdata to true data explained by incldued principal components
#as.matrix(PCAdata[,!(names(PCAdata) %in% c("DxPCR..Blood."))]) %*% t(PCAresult$rotation[,numbers])
#Rotate importances to predictor axis
PredictorImportance<-t(Trainedmodel$variable.importance) %*% t(PCAresult$rotation[,numbers])
observationNames<-attr(PredictorImportance,"dimnames")[[1]]
PredVec<-PredictorImportance[1:65]
names(PredVec)<-observationNames
#sort(PredictorImportance, decreasing=TRUE)
#transform Predictor Importance back to data frame
PredictorFrame<-data.frame(matrix(ncol = 65, nrow = 0))
names(PredictorFrame)<-attr(PredictorImportance,"dimnames")[[2]]
PredictorFrame[1,]<-PredictorImportance
#names(PredictorImportance)<-names(data[,!(names(data) %in% "DxPCR..Blood.")])

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
