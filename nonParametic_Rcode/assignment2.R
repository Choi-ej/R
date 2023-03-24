# extra packages to make random Bernoulli numbers
# install.packages("Rlab")
library(Rlab)

#Bernoulli distribution, mean
# remove all variables
rm(list=ls())
par(mfrow = c(2, 3)) # histogram 2*3
numbers = c(1,2,5,10,100,1000) # sample size

for (num in numbers) {
  mean_vector = numeric() # make empty vector
  for (i in 1:3000){
    mean_vector[i] <- mean(rbern(num,prob=0.05))
  }
  hist(mean_vector, freq=F, main=paste("Historgram of X_bar, n =",num),xlab="x_bar", breaks = 30)
}

rm(list=ls())
par(mfrow = c(2, 3))
numbers = c(1,2,5,10,100,1000)
#Bernoulli distribution, median
for (num in numbers) {
  median_vector = numeric() # make empty vector
  for (i in 1:20000){
    median_vector[i] <- median(rbern(num,prob=0.05))
  }
  hist(median_vector, freq=F, main=paste("Historgram of X_med, n =",num),xlab="x_med", breaks = 30)
}

numbers = c(1,2,5,10,100,1000)
# chisq distribution, mean
for (num in numbers) {
  mean_vector = numeric() # make empty vector
  for (i in 1:3000){
    mean_vector[i] <- mean(rchisq(num, df = 3))
  }
  hist(mean_vector, freq=F, main=paste("Historgram of X_bar, n =",num),xlab="x_bar", breaks = 30)
}

# chisq distribution, median
for (num in numbers) {
  median_vector = numeric() # make empty vector
  for (i in 1:3000){
    median_vector[i] <- median(rchisq(num, df = 3))
  }
  hist(median_vector, freq=F, main=paste("Historgram of X_med, n =",num),xlab="x_med", breaks = 30)
}

numbers = c(1,2,5,10,100,1000)
# uniform distribution, mean
for (num in numbers) {
  mean_vector = numeric() # make empty vector
  for (i in 1:3000){
    mean_vector[i] <- mean(runif(num,0,1))
  }
  hist(mean_vector, freq=F, main=paste("Historgram of X_bar, n =",num),xlab="x_bar", breaks = 30)
}

# uniform distribution, median
for (num in numbers) {
  median_vector = numeric() # make empty vector
  for (i in 1:3000){
    median_vector[i] <- median(runif(num,0,1))
  }
  hist(median_vector, freq=F, main=paste("Historgram of X_med, n =",num),xlab="x_med", breaks = 30)
}

#normal distribution, mean
for (num in numbers) {
  mean_vector = numeric() # make empty vector
  for (i in 1:3000){
    mean_vector[i] <- mean(rnorm(n = num, mean = 0,sd = 1))
  }
  hist(mean_vector, freq=F, main=paste("Historgram of X_bar, n =",num),xlab="x_bar", breaks = 30)
}

#normal distribution, median
for (num in numbers) {
  median_vector = numeric() # make empty vector
  for (i in 1:3000){
    median_vector[i] <- median(rnorm(n = num, mean = 0,sd = 1))
  }
  hist(median_vector, freq=F, main=paste("Historgram of X_med, n =",num),xlab="x_med", breaks = 30)
}
