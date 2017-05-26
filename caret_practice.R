# Load packages and datasets
library(caret)
library(kernlab)
library(ISLR)
library(ggplot2)
library(Hmisc)
library(gridExtra)
data(spam)
data(Wage)


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


# Plotting Pairplot
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
featurePlot(x=training[,c("age", "education", "jobclass")], y=training$wage, plot="pairs")

# Plotting data seperated by factor variable
qplot(age, wage, color=education, data=training) + # qplot{ggplot2}
  geom_smooth(method='lm', formula=y~x) # geom_smooth{ggplot2}

# Plotting density plot
qplot(wage, colour=education, data=training, geom="density")

# Plotting boxplot with samples
cutWage <- cut2(training$wage, g=3) # cut2{Hmisc}
p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot")) 
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol=2) # grid.arrange{gridExtra}

# Using table
table(cutWage, training$jobclass)
prop.table(table(cutWage, training$jobclass))
prop.table(table(cutWage, training$jobclass), 1)

