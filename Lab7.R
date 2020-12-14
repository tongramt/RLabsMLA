# Title     : Lab 7
# Objective : Linear Diiscriminant analysis
# Created by: ThomasGrant
# Created on: 02/12/2020
require("MASS")
library("MASS")
salmon <- read.table("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/salmon.txt")
head(salmon, h=10)
plot(salmon[,-1], col = as.factor(salmon.data[,1]))
install.packages("ellipse")
library("ellipse")
plot(salmon[,c(2,3)], col = as.factor(salmon[, 1]), xlim=c(50,190), ylim=c(290,530))
# Take a look at the whole data set, break it into your groups and use these groups means and covariances as your ellipse parameter
# level is the CI
lines(ellipse(cov(salmon[c(1:50), c(2, 3)]), centre = colMeans(salmon[c(1:50), c(2, 3)]),level = c(0.5)))
lines(ellipse(cov(salmon[c(51:100), c(2, 3)]), centre = colMeans(salmon[c(51:100), c(2, 3)]), level = 0.5), col = 2)
?ellipse
# Split the data into training and test sets
strain <- salmon.data[c(1:40, 51:90), ]
stest <- salmon.data[c(41:50, 91:100),]
# Perform LDA on the salmon data using the train and test groups created
lsol <- lda(strain[, c(2, 3)], grouping = strain[,1])
# prior shows the prior probabilities of the data beingin each group
lsol$prior
lsol$means
# Estimte the pooled covariance of the two groups in the salmon data
alaska_salmon <- strain[strain == "Alaska", c(2,3)]
canada_salmon <- strain[strain == "Canada", c(2,3)]
n_alaska <- length(alaska_salmon)
n_canada <- length(canada_salmon)
single_cov_num <- ((n_alaska - 1) * cov (alaska_salmon) + (n_canada - 1) * cov(canada_salmon) )
single_cov <- single_cov_num / ( length(strain[, 1]) - 2)
single_cov
# provides the scaling coefficients of the data
lsol$scaling
# Exercise 1: Explain the scaliing output.
# When you look at the data you can see that the fresh water values in Alaska are smaller and the
# marine values are larger. This means if you multiple an observations values by the values from Lsol$scaling
# you should get a negative value for alaska and positive for canada
# Exercise 2: classify a data point with values freshwater(120) and marine(380)
predict(lsol,c(120,380))
# Predict the classifications for the test data set
test_predict <- predict(lsol, stest[,c(2,3)])
# Exercise 3: Compute the accuracy of the LDA
lsol_class_agree <- table(test_predict$class, stest[,1])
lsol_sum_agree <- sum(diag(lsol_class_agree))
lsol_accuracy <- lsol_sum_agree/nrow(stest)*100
lsol_accuracy
# Using Cross Validation with LDA
# Cross validation tells you which points would be misclassified if they were the only point left out
lsol_cv <- lda(salmon.data[,c(2,3)], grouping = salmon.data[, 1], CV = TRUE)
# Shows which data is misclassified with colour being the actual class and shape being the predicted class
plot(salmon.data[, c(2, 3)], col = as.factor(salmon.data[, 1]), pch = as.numeric(lsol_cv$class))
# Use QDA
qsol <- qda(strain[, c(2,3)], grouping = strain[, 1])
qsol_predict <- predict(qsol, stest[, c(2, 3)])
qsol_class_agree <- table(qsol_predict$class, stest[,1])
qsol_sum_agree <- sum(diag(qsol_class_agree))
qsol_accuracy <- qsol_sum_agree/nrow(stest)*100
qsol_accuracy
# Show the covarance of the two salmon data points
cov(alaska_salmon)
cov(canada_salmon)
# Assess the performance of qda using Cross validation
qsol_cv <- qda(salmon.data[, c(2,3)], grouping = salmon.data[, 1], CV = TRUE)
plot(salmon.data[, c(2, 3)], col = as.factor(salmon.data[, 1]), pch = as.numeric(qsol_cv$class))
# Asssess the performance of the qda and lda using 50,25,25 rule
salmon_train <- salmon.data[c(1:25, 51:75), ]
salmon_test <- salmon.data[c(26:37, 76:87), ]
salmon_valid <- salmon.data[c(38:50, 88:100), ]
qsol_salmon <-  qda(salmon_train[, c(2,3)], grouping = salmon_train[, 1])
qsol_salmon_predict <- predict(qsol_salmon, salmon_test[, c(2, 3)])
qsol_salmon_valid <- predict(qsol_salmon, salmon_valid[, c(2, 3)])
qsol_test_class_agree <- table(qsol_salmon_predict$class, salmon_test[,1])
qsol_test_sum_agree <- sum(diag(qsol_test_class_agree))
qsol_test_accuracy <- qsol_test_sum_agree/nrow(salmon_test)*100
qsol_valid_class_agree <- table(qsol_salmon_valid$class, salmon_valid[,1])
qsol_valid_sum_agree <- sum(diag(qsol_valid_class_agree))
qsol_valid_accuracy <- qsol_valid_sum_agree/nrow(salmon_valid)*100
qsol_valid_accuracy
qsol_test_accuracy



