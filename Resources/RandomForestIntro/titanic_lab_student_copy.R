#install.packages("Hmisc")
library(Hmisc)

#####################
## 1. Read in data ##
#####################
titanic_data <- read.csv("contest_samples.csv")

######################################
## 2. Look at structure of the data ##
######################################
str(titanic_data)
titanic_data$survived <- as.factor(titanic_data$survived)
View(titanic_data)
describe(titanic_data)

#################
## 3. set seed ##
#################
set.seed(5)

########################################
## 4. Split training and testing data ##
########################################

## Randomly order samples
random_ordered <- titanic_data[sample(nrow(titanic_data)),]

## Determine the number of training samples
number_training_samples <- ceiling(nrow(random_ordered) * 0.9)
## Create training set
titanic_train <- random_ordered[1:number_training_samples,]
## Create testing set
titanic_test <- random_ordered[(number_training_samples + 1):nrow(random_ordered),]

## Save correct answers for testing set to test accuracy
test_answers <- data.frame(id = titanic_test$id, survived = titanic_test$survived)
head(test_answers)
summary(test_answers)
###########################
## 5. randomForest package ##
#############################
#install.packages("randomForest")
library(randomForest)

###################################
## 6. Dealing with missing data ##
###################################

## Omit samples with missing data
titanic_train_omit <- na.omit(titanic_train)
titanic_test_omit <- na.omit(titanic_test)

#############################
## 7. Random Forest Model ##
#############################
random_forest_omit <- randomForest(survived ~ pclass + sex + age + sibsp + parch + fare +
                                     embarked, data=titanic_train_omit,ntrees=1000,importance=TRUE)
  ## TODO: create a random forest model with the "omit" data

#########################
## 8. Predict accuracy ##
#########################
prediction_forest <- predict(random_forest_omit, titanic_test_omit)
solution_forest <- data.frame(id = titanic_test_omit$id, survived = prediction_forest)
comparison_omit <- merge(test_answers, solution_forest, by = "id", all = FALSE)
sum(comparison_omit$survived.x == comparison_omit$survived.y)/nrow(comparison_omit)

importance(random_forest_omit)

##############################
## 9. Random Forest Impute ##
##############################
## Impute missing values
titanic_train_impute <- rfImpute(survived ~ pclass + sex + age + 
                                   sibsp + parch + fare + embarked, titanic_train)
titanic_test_impute <- rfImpute(survived ~ pclass + sex + age + 
                                  sibsp + parch + fare + embarked, titanic_test)

random_forest_impute <- randomForest(survived ~ pclass + sex + sibsp + age + parch + fare +
                                     embarked, data=titanic_train_omit,ntrees=2000,replace=FALSE, nodesize=0, importance=TRUE)
  
prediction_impute <- predict(random_forest_impute, titanic_test_impute)
solution_impute <- data.frame(id = titanic_test$id, survived = prediction_impute)
comparison_impute <- merge(test_answers, solution_impute, by = "id", all = FALSE)
sum(comparison_impute$survived.x == comparison_impute$survived.y)/nrow(comparison_impute)

importance(random_forest_impute)

#####################
## 10. Competition ##
#####################
contest_samples_test <- read.csv("~/Downloads/contest_samples.csv")
## Do any necessary modifications on this data to deal with 
## missing data points or create new features


#my_final_classifier <- ## TODO: add your final answer here
prediction <- predict(my_final_classifier, contest_samples_test)
sum(prediction == test_answers$survived)/nrow(test_answers)




