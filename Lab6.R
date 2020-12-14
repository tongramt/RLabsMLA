# Title     : Lab 6
# Objective : K-Nearest Neighnours in R
# Created by: ThomasGrant
# Created on: 02/12/2020
# Exercise 1:
# Generate multivariatenormal distribution with 900 observations and mean (0,0)
# and sig (10,3,3,2)
require("MASS")
library("MASS")
ex1_mean <- c(0,0)
ex1_sig <- matrix(c(10,3,3,2), nrow = 2, ncol = 2, byrow = TRUE)
simA <- mvrnorm(n=900, ex1_mean, ex1_sig)
# Generate multivariatenormal distribution with 900 observations and mean (5,3)
# and sig (12,2,2,15)
ex2_mean <- c(5,3)
ex2_sig <- matrix(c(12,2,2,15), nrow = 2, ncol = 2, byrow = TRUE)
simB <- mvrnorm(n=900, ex2_mean, ex2_sig)
# Bind the two datasets together
simT <- rbind(simA,simB) # Stacks the two sims on top of each other
class_ind <- rep(c(1, 2), each = 900) # categorises the observations based on which sim they belonged to
simT <- cbind(simT, class_ind)# categorises the observations based on which sim they belonged to
plot(simT[,1], simT[,2], col = as.factor(simT[, 3]))
# Train and test the data
index_train <- c(1:300, 1:300 + 900)
index_test <- c(1:300 + 300, 1:300 + 900 + 300)
index_valid <- c(1:300 + 600, 1:300 + 900 + 600)
train <- simT[index_train, 1:2]
test <- simT[index_test, 1:2]
valid <- simT[index_valid, 1:2]
install.packages("class")
library(class)
result <- knn(train = train, test = test, cl= simT[index_train, 3], k=3) #cl = the classification parameters
result
class_agree <- table(result, simT[index_test,3])
class_agree
sum_agree <- sum(diag(class_agree))
sum_agree
adj_agree <- (nrow(test) - sum_agree) / nrow(test)
adj_agree
kmax <- 50 # Max number of nearest neighbours
k <- 1:kmax # setting k as a vector of length=kmax
p <- rep(0, kmax)
ntest <- nrow(test) # number of rows in test
k_summary <- cbind(k, p)
colnames(k_summary) <- c("k","% misclassified")
for(i in 1:kmax){
result <- knn(train, test, cl = simT[index_test, 3], k = i)
class_agree <- table(result, simT[index_test,3])
sum_agree <- sum(diag(class_agree))
k_summary[i, 2] <- (ntest - sum_agree) / ntest
}
k_summary[1:10, ]
?plot
plot(k_summary[,1], k_summary[,2], type = 'l')
k_summary[10:20,]
which.min(k_summary[,2])
# Confirm the best k using
k_validate <- cbind(k, p)
colnames(k_validate) <- c("k","% misclassified")
for(i in 1:kmax){
  result <- knn(train, test, cl = simT[index_valid, 3], k = i)
  class_agree <- table(result, simT[index_test,3])
  sum_agree <- sum(diag(class_agree))
  k_validate[i, 2] <- (ntest - sum_agree) / ntest
}
which.min(k_validate[,2])
# Perform a knearest neighbour analysis on the iris data set
plot(iris[,1:4], col = as.factor(iris[,5]))
iris_index_train <-c(1:25,1:25+50,1:25+100)
iris_index_test <- c(1:12+25,1:12+75,1:12+125)
iris_index_validate <- c(1:13+37,1:13+87,1:13+137)
iris_train <- iris[iris_index_train,1:4]
iris_test <- iris[iris_index_test,1:4]
iris_validate <- iris[iris_index_validate,1:4]
iris_k_max = 25
k <- 1:iris_k_max
p <- rep(0, iris_k_max)
ntest <- nrow(iris_test) # number of rows in test
iris_k_test_summary <- cbind(k, p)
colnames(iris_k_test_summary) <- c("k","% misclassified")
for(i in 1:kmax){
  result <- knn(iris_train, iris_test, cl = iris_mat[iris_index_train, 5], k = i)
  class_agree <- table(result, iris[iris_test,5])
  sum_agree <- sum(diag(class_agree))
  iris_k_test_summary[i, 2] <- (ntest - sum_agree) / ntest
}
which.min(iris_k_test_summary[,2])






