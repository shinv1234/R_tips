# load libraries
library(mice)
library(Hmisc)
library(DMwR)
library(rpart)

# load dataset
data("BostonHousing", package="mlbench")

# backup dataset
original <- BostonHousing 

# explore dataset
summary(BostonHousing)
head(BostonHousing)

# generating missing values
set.seed(100)
BostonHousing[sample(1:nrow(BostonHousing), 40), "rad"] <- NA
BostonHousing[sample(1:nrow(BostonHousing), 40), "ptratio"] <- NA
head(BostonHousing)

# Check missing values
anyNA(BostonHousing)
md.pattern(BostonHousing) # md.pattern{mice}

# Imputation with mean / median / mode
impute(BostonHousing$ptratio, mean) # impute{Hmisc}
impute(BostonHousing$ptratio, median)
impute(BostonHousing$ptratio, 20)

# BostonHousing$ptratio[is.na(BostonHousing$ptratio)] <- mean(BostonHousing$ptratio, na.rm = T)
# BostonHousing$ptratio[is.na(BostonHousing$ptratio)] <- median(BostonHousing$ptratio, na.rm = T)
# BostonHousing$ptratio[is.na(BostonHousing$ptratio)] <- 20
  
# accuracy(imputation with mean)
actuals1 <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds1 <- rep(mean(BostonHousing$ptratio, na.rm = T), length(actuals))
regr.eval(actuals1, predicteds1) # regr.eval{DMwR}

# accuracy(imputation with median)
actuals2 <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds2 <- rep(median(BostonHousing$ptratio, na.rm = T), length(actuals))
regr.eval(actuals2, predicteds2) # regr.eval{DMwR}

# KNN imputation(without target variable "medv")
knnOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"]) # knnImputation {DMwR}
anyNA(knnOutput) # Check for missing values
md.pattern(knnOutput)

# accuracy(KNN imputation)
actuals3 <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds3 <- knnOutput[is.na(BostonHousing$ptratio), "ptratio"]
regr.eval(actuals3, predicteds3) # regr.eval{DMwR}

# 
class_mod <- rpart(rad ~ . - medv, 
                   data = BostonHousing[!is.na(BostonHousing$rad), ], 
                   method = "class", 
                   na.action = na.omit)
anova_mod <- rpart(ptratio ~ . - medv, 
                   data = BostonHousing[!is.na(BostonHousing$ptratio), ], 
                   method = "anova", 
                   na.action = na.omit)
rad_pred <- predict(class_mod,
                    BostonHousing[is.na(BostonHousing$rad), ])
ptradio_pred <- predict(anova_mod,
                        BostonHousing[is.na(BostonHousing$ptratio), ])

# accuracy( )
actuals4 <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds4 <- ptradio_pred
regr.eval(actuals4, predicteds4) # regr.eval{DMwR}

