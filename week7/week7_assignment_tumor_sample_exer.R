setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week7/")
#rm(cmd, cmd.frame, mtcars.sc, cophenetic.d, d, hc, pca)
library(ipred)
library(ElemStatLearn)
nci = nci
# We are going to remove some predictors in a prefiltering. Strictly
# speaking this is a BAD THING. For the purposes of a tutorial however it
# makes sense to work with a smaller dataset. Note how the apply function
# calculates sd for each row without an explicit loop.
gene.sd = apply(nci, 1, sd)
# pick the gens with the highest variance. NB the filtering should not
# relate to the response or else the error estimate will be biased.
high.varying.genes = order(gene.sd, decreasing = T)[1:400]
nci = nci[high.varying.genes, ]
cl = as.factor(colnames(nci))
nci = t(nci)
nci = data.frame(cl = cl, nci)

head(nci[, 1:5])
# there are 64 rows of data, that is 64 cancer samples
dim(nci)
# we randomly pick two thirds of those rows
s = sample(1:64, 48)
# we train the model on 2/3rds of the data.. this may take a moment
knn.model = ipredknn(cl ~ ., data = nci[s, ], k = 1)
#  we make prediction on the remaining test data
knn.pred = predict.ipredknn(knn.model, newdata = nci[-s, ], type = "class")
# we compare the prediction to the true class
data.frame(knn.pred = knn.pred, cl = cl[-s])
# the correct prediction rate is calculated
mean(knn.pred == cl[-s])

knn.mymodel = function(formula, data, k = 1) ipredknn(formula, data, k = k)
knn.mypred = function(object, newdata) predict.ipredknn(object, newdata, type = "class")
# this takes about 20 secs on my computer as the function loops through
# training and testing many models on a fairly large dataset. This is one
# reason R is sometimes eschewed for machine learning problems.
knn.cv = errorest(cl ~ ., data = nci, model = knn.mymodel, predict = knn.mypred)
knn.cv

# note just the class labels are extracted from the lda model object
lda.mypred = function(object, newdata) predict(object, newdata = newdata)$class
lda.cv = errorest(cl ~ ., data = nci, model = lda, predict = lda.mypred)
lda.cv