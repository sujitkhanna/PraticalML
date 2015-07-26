Summary:

In this project, the goal is to analyze data from accelerometers on the belt, forearm, arm, and dumbell of six participants. They were asked to perform barbell lifts correctly and incorrectly in five different ways. For more information see the “Weight Lifting Exercises Dataset” in the following location:
http://groupware.les.inf.puc-rio.br/har

The goal of this machine learning algorithm is to predict the manner in which the participants did the exercise meaning  to predict the “classe” variable found in the training set. The prediction model will then be used to predict twenty different test cases, as provided in the testing dataset.

Analysis:

Loading the Libraries:-

library(caret)

library(randomForest)

library(rpart)



Getting Data-
The training and testing datasets used in the analysis may be found as follows:
We begin by loading the required libraries and reading in the training and testing datasets, assigning missing values to entries that are currently 'NA' or blank.

data_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
data_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url = data_train, destfile = "pml-training.csv",method = "wget")
download.file(url = data_test, destfile = "pml-testing.csv",method = "wget")

Loading the data:

pml.training <- read.csv("pml-training.csv")
pml.testing <- read.csv("pml-testing.csv")

Cull unnecessary variables:

except.these <- grep("kurtosis|skewness|max|min|amplitude|avg|stddev|var", names(pml.training))
except.these <- c(except.these,c(1:7))
pml.training1 <- pml.training[,-except.these]
sum(is.na(pml.training1))  ## Check for NAs. 0 is nice.
pml.testing1 <- pml.testing[,-except.these]
which( colnames(pml.training1)=="classe" )
pml.training1[, -53] <- as.data.frame(lapply(pml.training1[,-53],as.numeric))
pml.testing1[, -53] <- as.data.frame(lapply(pml.testing1[,-53],as.numeric))


Data Splitting:-
We now go ahead and split the data, 80% for training and 20% for testing.

set.seed(42)
inTrain <- createDataPartition(y=pml.training1$classe, p=0.80, list=FALSE)
training <- pml.training1[inTrain,]
testing <- pml.training1[-inTrain,]
training[1,53]
class(training$classe)
dim(training);dim(testing)

We have now randomized training and equally distributed amongst the five classes of our outcome variable

Model Data with Trees:

We will now train a model the Classification and Regression Tree (CART) algorithm (method='rpart') and use a 10-fold cross-validation 

set.seed(42)
system.time( barb.tr <- train(classe ~ .,method="rpart",data=training, trControl = trainControl(method = "cv", number = 10)))

Analysis of Trees:
Our tree model is not better than chance. The in-sample error rate (from a 10-fold cross-validation) is 49.8% (barb.tr$results). The out-of-sample error rate is 50.2%.

barb.tr$finalModel

barb.tr$resample ## error rate of each fold
barb.tr$results ## cp = complexity parameter

confusionMatrix(barb.tr)

### Test the Tree Model ###
pred.te<- predict(barb.tr,newdata=testing)
testing$predtrRight <- pred.te==testing$classe
table(pred.te,testing$classe)
1-sum(testing$predtrRight)/nrow(testing)  ## out of sample error rate


Model with Random Forest:
This Random Forest model uses a training set of 20% to keep the processing time down (prox = TRUE). As Random forest that uses 80% of training set take much too long 

Split the Data (20/80):

set.seed(42)
inTrain <- createDataPartition(y=pml.training1$classe, p=0.20, list=FALSE)
training20 <- pml.training1[inTrain,]
testing80 <- pml.training1[-inTrain,]
training20[1,53]
class(training20$classe)
dim(training20);dim(testing80)

Train the Random Forest model (20%)

set.seed(42)
system.time( barb.rf <- train(classe ~., data=training20,method="rf",prox=TRUE) )
####     user   system  elapsed 
#### 3395.666   34.611 3509.028 
barb.rf$finalModel
varImpPlot(barb.rf$finalModel)

### Get features in order of importance ###
imp <- as.data.frame(barb.rf$finalModel$importance)
imp <- cbind(feature = rownames(imp), imp)
rownames(imp)<-NULL
head(imp[with(imp, order(-MeanDecreaseGini)), ],10)

In this case out of bag error rate is 2.39%


we now Predicting the random forest model with the test set of 80%. The out-of-sample error rate is 2.46%

pred <- predict(barb.rf,testing80)
testing80$predRight <- pred==testing80$classe
table(pred,testing80$classe)

### Out-of-Sample error rate ###
1-sum(testing80$predRight)/nrow(testing80)

WE now use Random Forest Model with 80% training set but doesnt calculate proximities
(prox=False)

set.seed(42)
system.time( barb80.rf <- train(classe~ .,data=training,method="rf",prox=FALSE) )
####     user   system  elapsed 
#### 8358.456   66.568 8647.038 
### Get features in order of importance ###
imp80 <- as.data.frame(barb80.rf$finalModel$importance)
imp80 <- cbind(feature = rownames(imp80), imp80)
rownames(imp80)<-NULL
head(imp80[with(imp80, order(-MeanDecreaseGini)), ],10)
varImpPlot(barb80.rf$finalModel)
barb80.rf$finalMode

PREDICTION with the test set (20%)
The features' importance branches differently than the previous model. It attains an out-of-bag error rate of 0.5% and an out-of-sample error rate of 0.6%.

pred <- predict(barb80.rf,testing)
testing$predRight <- pred==testing$classe
table(pred,testing$classe)
table(testing$predRight,testing$classe)
### Out-of-Sample Error rate ###
1-sum(testing$predRight)/nrow(testing)

Submit Results:

The Last Part of the analysis is to use the model to predict the 'classe' for testing data and submitting the results

### Submission script ###
answers <- as.character(pred)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)

plot(sort(modFit$importance),training) 
fancyRpartPlot(modFit)
summary(modFit)

Out-of-bag Error Estimate without Cross Validation:
In random forests, there is no need for cross-validation or a separate test set to get an unbiased estimate of the test set error. Instead we use the out-of-bag error estimate.










