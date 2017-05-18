# apply
(m <- matrix(data=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3))
apply(m, 1, mean) # 1: row
apply(m, 2, mean) # 2: column
apply(m, 2, function(x) length(x[x<0]))
apply(m, 2, function(x) is.matrix(x))
apply(m, 2, is.vector)
apply(m, 2, function(x) mean(x[x>0]))


# lapply(list apply)
(l<-list(a=1:3, b=25:29))
head(cars)
lapply(cars, mean)
lapply(l, mean)


# sapply(simplified apply)
(l<-list(a=1:3, b=25:29))
head(cars)
sapply(cars, function(x) x^2)
sapply(l, mean)


# tapply(table apply???)
head(chickwts)
tapply(chickwts$weight, chickwts$feed , mean)

# mapply
a <- c(2,4,6,8)
b <- 4:1
mapply(rep, a, b) # rep: repeat (ex: rep(1:4, 2); rep(1:4, c(2,2,2,2)))
mapply(mean, a, b)
mapply(mean, b, a)
rep(1:4, c(2,2,2,2))

# vapply
