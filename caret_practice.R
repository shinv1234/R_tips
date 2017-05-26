# Load packages and dataset
library(caret)
library(kernlab)
data(spam)

# Split train and test dataset
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# 10-Fold Crossvalidation
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
folds.test <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE)
str(folds)
str(folds.test)

# 10 Resampling
resamples <- createResample(y=spam$type, times=10, list=TRUE)
str(resamples)

# Time series
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)
str(folds)




