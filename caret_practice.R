# Load packages and datasets
library(caret)
library(kernlab)
library(ISLR)
library(ggplot2)
library(Hmisc)
library(gridExtra)
library(splines)
data(spam)
data(Wage)
data(faithful)

### Spliting Data
# Spliting Train and Test dataset
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# 10-Fold Cross Validation
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
folds.test <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE)
str(folds)
str(folds.test)

# 10 Resampling
resamples <- createResample(y=spam$type, times=10, list=TRUE)
str(resamples)

# Time Series Slicing
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)
str(folds)

### Ploting data
# Plotting Pairplot
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
featurePlot(x=training[,c("age", "education", "jobclass")], y=training$wage, plot="pairs")

# Plotting Data Seperated By Factor variable
qplot(age, wage, color=education, data=training) + # qplot{ggplot2}
  geom_smooth(method='lm', formula=y~x) # geom_smooth{ggplot2}

# Plotting Density Plot
qplot(wage, colour=education, data=training, geom="density")

# Plotting Boxplot With Samples
cutWage <- cut2(training$wage, g=3) # cut2{Hmisc}
p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot")) 
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol=2) # grid.arrange{gridExtra}

# Using Table
table(cutWage, training$jobclass)
prop.table(table(cutWage, training$jobclass))
prop.table(table(cutWage, training$jobclass), 1)


### Preprocessing
# Applying Standardization
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
pre0bj <- preProcess(training[,-58], method=c("center", "scale")) # preProcess{caret}
str(pre0bj)
preprocessed_training <- predict(pre0bj, training[,-58]) # apply preprocessing
trainCapAves <- predict(pre0bj, training[,-58])$capitalAve
testCapAves <- predict(pre0bj, testing[,-58])$capitalAve
rbind(train = c(mean = mean(trainCapAves), std = sd(trainCapAves)),
      test = c(mean = mean(testCapAves), std = sd(testCapAves)))

# Applying BoxCox
pre0bj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAves <- predict(pre0bj, training[,-58])$capitalAve
par(mfrow=c(1, 2)); hist(trainCapAves); qqnorm(trainCapAves)

# Imputing missing datas using knn imputation in preProcessing
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05) == 1
training$capAve[selectNA] <- NA
pre0bj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(pre0bj, training[-58])$capAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)
quantile(capAve - capAveTruth)


### 
# Creating Dummy Variables
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

# Checking Near-Zero Variables
nearZeroVar(training, saveMetrics=TRUE) # nearZeroVar{caret}
'freqRatio: Frequency of most measured values / The frequency of the second most measured value
percentUnique: 
zeroVar: Variance = 0
nzv: near-zero-variance'

# Creating Splines
bsBasis <- bs(training$age, df=3) # Creating Basis function
lm1 <- lm(wage ~ bsBasis, data=training)
summary(lm1)
par(mfrow=c(1, 1))
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)


# Multicore Parallel Processing (Pass..)


# PCA
prComp <- prcomp(log10(spam[,-58]+1))
head(prComp$rotation[, 1:5])
typeColor <- ((spam$type == "spam")*1 + 1)
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

# Pass...
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
preProc <- preProcess(log10(training[,-58] + 1), method="pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58] + 1))
modelFit <- train(training$type ~ ., method='glm', data=trainPC)
confusionMatrix(testing$type,predict(modelFit,testPC))
confusionMatrix(testing$type,predict(modelFit,testPC))

modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))


### Regression
# Predicting Regression
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith <- faithful[inTrain, ]; testFaith <- faithful[-inTrain, ]
lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

# Predicting New Data
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

# Plotting Regression Line
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",
     ylab="Duration", main = "Train")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",
     ylab="Duration", main = "Test")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# RMSE, RMAE
c(trainRMSE = sqrt(sum( (lm1$fitted - trainFaith$eruptions) ^ 2) ), 
  testRMSE = sqrt(sum( (predict(lm1, newdata=testFaith) - testFaith$eruptions) ^ 2) ))
c(trainRMAE = sqrt(sum( abs(lm1$fitted - trainFaith$eruptions) ) ), 
  testRMAE = sqrt(sum( abs(predict(lm1, newdata=testFaith) - testFaith$eruptions) ) ))

# Prediction Line and Interval Line
par(mfrow=c(1,1))
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting, pred1, type='l', col=c(1,2,2), lty=c(1,1,1), lwd=3)

# Regression Diagnostics
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain, ]; testing <- Wage[-inTrain, ]
modFit <- train(wage ~ age + jobclass + education, method="lm", data=training)
finMod <- modFit$finalModel
predwage <- predict(modFit, newdata=testing)
par(mfrow=c(2,2))
plot(finMod, pch=19, cex=0.5, col="#00000010")

qplot(finMod$fitted, finMod$residuals, color=race, data=training)

par(mfrow=c(1,1))
plot(finMod$residuals, pch=19, cex=0.5)


