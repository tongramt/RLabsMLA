# Title     : Assignment 1
# Objective :
# Created by: ThomasGrant
# Created on: 03/12/2020
heart <- read.csv("/Users/ThomasGrant/Downloads/heart_data(1).csv")
# In the description of the data it says that the values for column 9 are between 0 and 3
# and in the dataset they are between 1 and 4
for(i in 1:nrow(heart)){
  heart[i,9] <- heart[i,9]-1
}
as.integer(heart[,9])
# Break down data into binary and non binary variables
binary_heart <- heart[, c(2,5,7,10)]
non_binary_heart <- heart[, c(1,3,4,6,8,10)]
plot(non_binary_heart)
# Analyse what components provide us with good insight into the data
pairs(heart[,c(1:9)], col=heart[,10])
# Using unsupervised learning methods 30%
# 1: Principal Component Analysis
# Run PCA on the various aspects of the data without the heart disease factor as that will be used later
heart_full_pca <- prcomp(heart[,1:9])
heart_real_pca <-  prcomp(non_binary_heart[,1:5])
heart_binary_pca <- prcomp(binary_heart[,1:3])
#Check to see the results of PCA on the broken up heart data
summary(heart_full_pca)
summary(heart_real_pca)
# make usable datasets for the code going forward
new_full_heart <- predict(heart_full_pca)
new_real_heart <- predict(heart_real_pca)
# plotting the PCAs
full_pca_plot <- plot(new_full_heart[,1], new_full_heart[,2], type=, xlab="PC1", ylab="PC2", col=as.integer(heart[,10]))
real_pca_plot <- plot(new_real_heart[,1], new_real_heart[,2], type=, xlab="PC1", ylab="PC2", col=as.integer(heart[,10]))

# 2: Hierarchical Clustering
# Using supervised learning Methods
# K-Nearest Neighbours
# LDA
# QDA
# K-Means Testing
plot(pairs(heart[,1:9]))
binary_heart <- heart[, c(2,5,7,10)]
non_binary_heart <- heart[, c(1,3,4,6,8,9,10)]
plot(pairs(non_binary_heart))
