# load libraries
library(mice)
library(Hmisc)
library(DMwR)
library(rpart)
library(mice)

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
md.pattern(knnOutput) # md.pattern{mice}

# accuracy(KNN imputation)
actuals3 <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds3 <- knnOutput[is.na(BostonHousing$ptratio), "ptratio"]
regr.eval(actuals3, predicteds3) # regr.eval{DMwR}

# CART imputation from rpart
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

# accuracy(CART imputation for "ptratio")
actuals4 <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds4 <- ptradio_pred
regr.eval(actuals4, predicteds4) # regr.eval{DMwR}

# accuracy(CART imputation for "rad")
actuals5 <- original$rad[is.na(BostonHousing$rad)]
predicteds5 <- as.numeric(colnames(rad_pred)[apply(rad_pred, 1, which.max)])
mean(actuals5 != predicteds5)

# MICE imputation(based on random forests)
miceMod <- mice(BostonHousing[, !names(BostonHousing) %in% "medv"], method="rf") # mice{mice} 
miceMod

# generate the completed data
miceOutput <- complete(miceMod)  # complete{mice}
anyNA(miceOutput)
md.pattern(miceOutput) # md.pattern{mice}

# accuracy(MICE imputation for "ptratio")
actuals6 <- original$ptratio[is.na(BostonHousing$ptratio)]
predicteds6 <- miceOutput[is.na(BostonHousing$ptratio), "ptratio"]
regr.eval(actuals6, predicteds6) # regr.eval{DMwR}

# accuracy(MICE imputation for "rad")
actuals7 <- original$rad[is.na(BostonHousing$rad)]
predicteds7 <- miceOutput[is.na(BostonHousing$rad), "rad"]
mean(actuals7 != predicteds7) 