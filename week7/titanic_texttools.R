setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week7/")
library(RTextTools)
data <- read.csv("train.csv")
#Remove name
data <- data[-4]
#Remove Ticket
data <- data[-8]
#change numeric classes to add text to them since this is a "text tool"
Pclass <- as.vector(apply(as.matrix(data[3], mode="character"),1,paste,"Pclass",sep="",collapse=""))
Age <- as.vector(apply(as.matrix(data[5], mode="character"),1,paste,"Age",sep="",collapse=""))
SibSp <- as.vector(apply(as.matrix(data[6], mode="character"),1,paste,"SibSp",sep="",collapse=""))
Parch <- as.vector(apply(as.matrix(data[7], mode="character"),1,paste,"Parch",sep="",collapse=""))
Fare <- as.vector(apply(as.matrix(data[8], mode="character"),1,paste,"Fare",sep="",collapse=""))
#create the training data
training_data <- cbind(data[1],data[2],Pclass,data[4],Age,SibSp,Parch,Fare,data[9],data[10])
#get the labels
training_codes <- training_data[2]
#remove labels from training data
training_data <- training_data[-2]

require(tm)
matrix <- create_matrix(training_data, language="english", removeNumbers=FALSE, stemWords=FALSE, removePunctuation=FALSE, weighting=weightTfIdf)
container <- create_container(matrix,t(training_codes),trainSize = 1:891,virgin=FALSE)
models <- train_models(container, algorithms=c("MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"))
results <- classify_models(container, models)

analytics <- create_analytics(container, results)
analytics@ensemble_summary
analytics@algorithm_summary
analytics@label_summary

cross_validate(container, 4, algorithm=c("MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"))
SVM <- cross_validate(container, 4, "SVM")
GLMNET <- cross_validate(container, 4, "GLMNET")
MAXENT <- cross_validate(container, 4, "MAXENT")
SLDA <- cross_validate(container, 4, "SLDA")
BAGGING <- cross_validate(container, 4, "BAGGING")
BOOSTING <- cross_validate(container, 4, "BOOSTING")
RF <- cross_validate(container, 4, "RF")
NNET <- cross_validate(container, 4, "NNET")
TREE <- cross_validate(container, 4, "TREE")

performance <- cbind(SVM$meanAccuracy,GLMNET$meanAccuracy,MAXENT$meanAccuracy,SLDA$meanAccuracy,
      BAGGING$meanAccuracy,BOOSTING$meanAccuracy,RF$meanAccuracy,NNET$meanAccuracy
      ,TREE$meanAccuracy)

colnames(performance) <- c('SVM','GLMNET','MAXENT','SLDA','BAGGING','BOOSTING','RF','NNET','TREE')

matrix <- create_matrix(training_data, language="english", removeNumbers=FALSE, stemWords=FALSE, removePunctuation=FALSE, weighting=weightTfIdf)
container <- create_container(matrix,t(training_codes),trainSize = 1:891,virgin=FALSE)
models <- train_models(container, algorithms=c("SVM","GLMNET","SLDA","BAGGING","BOOSTING","RF"))
results <- classify_models(container, models)

analytics <- create_analytics(container, results)
analytics@ensemble_summary
analytics@algorithm_summary
analytics@label_summary
analytics@document_summary$CONSENSUS_CODE
write.csv(analytics@document_summary, "DocumentSummary.csv")

testdata <- read.csv("test.csv")
#testdata <- testdata[-3]
#testdata <- testdata[-7]
library(plyr)
newdata <- rbind.fill(data, testdata)
newdata <- newdata[-4]
#Remove Ticket
newdata <- newdata[-8]
Pclass <- as.vector(apply(as.matrix(newdata[3], mode="character"),1,paste,"Pclass",sep="",collapse=""))
Age <- as.vector(apply(as.matrix(newdata[5], mode="character"),1,paste,"Age",sep="",collapse=""))
SibSp <- as.vector(apply(as.matrix(newdata[6], mode="character"),1,paste,"SibSp",sep="",collapse=""))
Parch <- as.vector(apply(as.matrix(newdata[7], mode="character"),1,paste,"Parch",sep="",collapse=""))
Fare <- as.vector(apply(as.matrix(newdata[8], mode="character"),1,paste,"Fare",sep="",collapse=""))
#create the training data
new_training_data <- cbind(newdata[1],newdata[2],Pclass,newdata[4],Age,SibSp,Parch,Fare,newdata[9],newdata[10])
#get the labels
new_training_codes <- new_training_data[2]
#remove labels from training data
new_training_data <- new_training_data[-2]
require(tm)
matrix <- create_matrix(new_training_data, language="english", removeNumbers=FALSE, stemWords=FALSE, removePunctuation=FALSE, weighting=weightTfIdf)
container <- create_container(matrix,t(new_training_codes),trainSize = 1:891,testSize = 892:1309,virgin=FALSE)
#accidentally left in all algorithms, but received better results 0.78469
models <- train_models(container, algorithms=c("MAXENT","SVM","GLMNET","SLDA","TREE","BAGGING","BOOSTING","RF"))
#0.77990 performance
models <- train_models(container, algorithms=c("SVM","GLMNET","SLDA","BAGGING","BOOSTING","RF"))
#try with just top 2 (don't understand how GLMNET is over 100%), received same results as previous
models <- train_models(container, algorithms=c("BAGGING","BOOSTING"))
#try adding 3 more performers - still same results
models <- train_models(container, algorithms=c("BAGGING","BOOSTING","TREE","RF"))

results <- classify_models(container, models)
analytics <- create_analytics(container, results)



#predict(models[[1]],newdata= test_matrix)
#test_size = length(testdata)
#testContainer <- create_container(test_matrix, labels=rep(0,418), testSize=1:418, virgin=FALSE)
testContainer <- create_container(matrix,t(new_training_codes),testSize = 892:1309,virgin=TRUE)
# predict
results <- classify_models(testContainer, models)
results
analytics <- create_analytics(testContainer, results)
write.csv(analytics@document_summary$CONSENSUS_CODE, "DocumentSummary4.csv")
submission1 <- read.csv("submission.csv")
submission2 <- read.csv("DocumentSummary4.csv")
newsubmission <- cbind(submission1,submission2)
newsubmission <- newsubmission[c('PassengerId','x')]
write.table(newsubmission, file = "submission5.csv", col.names = TRUE, row.names = FALSE, sep = ",")
