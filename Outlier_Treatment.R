# Load package
library(car)
library(outliers)

# Explore dataset
summary(cars)
str(cars)
head(cars)

# Back up original data
original <- cars

# Inject outlier into data.
cars1 <- cars[1:30, ]
cars_outliers <- data.frame(speed = c(19, 20, 20, 20, 20), 
                            dist = c(190, 186, 210, 220, 218))
cars2 <- rbind(cars1, cars_outliers)

# Understanding the implications of outliers
par(mfrow=c(1,2))
plot(cars2$speed, cars2$dist, 
     xlim=c(0, 28), ylim=c(0, 230), 
     main="With Outliers",
     xlab="speed", ylab="dist",
     pch="*", col="red", cex=2)
abline(lm(dist ~speed, data=cars2),
       col="blue", lwd=3, lty=2)

plot(cars1$speed, cars1$dist, 
     xlim=c(0, 28), ylim=c(0, 230), 
     main="Outliers removed \n A much be better fit!",
     xlab="speed", ylab="dist",
     pch="*", col="red", cex=2)
abline(lm(dist ~speed, data=cars1),
       col="blue", lwd=3, lty=2)


url <- "https://raw.githubusercontent.com/selva86/datasets/master/ozone.csv" 
ozone <- read.csv(url)  # import data
write.csv(ozone, 'C:/Users/a/R_tips/ozone.csv')

outlier_values <- boxplot.stats(ozone$pressure_height)$out  # outlier values.
par(mfrow=c(1,1))
boxplot(inputData$pressure_height, main="Pressure Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


colnames(ozone)
# For categorical variable
par(mfrow=c(1,2))
boxplot(ozone_reading ~ Month, 
        data=ozone, main="Ozone reading across months")  
boxplot(ozone_reading ~ Day_of_week, 
        data=ozone, main="Ozone reading for days of week") 


# Category boundaries
pretty(ozone$pressure_height)
# Continuous variable to categorical variable
head(
  cut(ozone$pressure_height, pretty(ozone$pressure_height))
  )

# Compare
par(mfrow=c(1,2))
boxplot(ozone_reading ~ pressure_height, 
        data=ozone, main="Boxplot for Pressure height (continuos var) vs Ozone")
boxplot(ozone_reading ~ cut(pressure_height, pretty(ozone$pressure_height)), 
        data=ozone, main="Boxplot for Pressure height (categorial) vs Ozone", 
        cex.axis=0.5)

# Cooks Distance
mod <- lm(ozone_reading ~ ., data=ozone)
cooksd <- cooks.distance(mod)

par(mfrow=c(1,1))
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

# Check influential rows
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
head(ozone[influential, ])  

# Outliers Test
outlierTest(mod) # outlierTest{car}

# Find largest value
set.seed(1234)
y=rnorm(100)
outlier(y)
outlier(y,opposite=TRUE)
dim(y) <- c(20,5)  # convert it to a matrix
outlier(y)
outlier(y,opposite=TRUE)

# Find observations above the percentile
set.seed(1234)
x = rnorm(10)
scores(x)  # z-scores => (x-mean)/sd
scores(x, type="chisq")  # chi-sq scores => (x - mean(x))^2/var(x)
scores(x, type="t")  # t scores
scores(x, type="chisq", prob=0.9)  # beyond 90th %ile based on chi-sq
scores(x, type="chisq", prob=0.95)  # beyond 95th %ile
scores(x, type="z", prob=0.95)  # beyond 95th %ile based on z-scores
scores(x, type="t", prob=0.95)  # beyond 95th %ile based on t-scores

# Replacing outliers
x <- ozone$pressure_height
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

