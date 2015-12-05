setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week7/")
library(caret)
library(randomForest)
trainSet <- read.table("train.csv", sep = ",", header = TRUE)
testSet <- read.table("test.csv", sep = ",", header = TRUE)
head(trainSet)
head(testSet)
table(trainSet[,c("Survived", "Pclass")])
table(trainSet[,c("Survived", "Sex")])
table(trainSet[,c("Survived", "Age")])
table(trainSet[,c("Survived","Sex", "Age")])
table(trainSet[,c("Survived", "SibSp")])
table(trainSet[,c("Survived", "Embarked")])
table(trainSet[,c("Survived", "Embarked", "Pclass")])
library(fields)
bplot.xy(trainSet$Survived, trainSet$Age)
summary(trainSet$Age)
# Comparing Survival Rate and Fare
bplot.xy(trainSet$Survived, trainSet$Fare)
summary(trainSet$Fare)
# Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)
# Set a random seed (so you will get the same results as me)
set.seed(42)
# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +   
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = trainSet, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
)
model
testSet$Survived <- predict(model, newdata = testSet)
summary(testSet)
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)
testSet$Survived <- predict(model, newdata = testSet)
submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
