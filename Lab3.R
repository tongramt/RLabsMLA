# Title     : Lab 3
# Objective : Write and annotate code for lab 3
# Created by: ThomasGrant
# Created on: 26/10/2020

# Checking if I had the Iris dataset then printing the first 5 entries
# to see which ones were numeric for calculations
?iris
dim(iris)
head(iris,n = 5)
fisher1 <- prcomp(iris[,1:4])
newiris <- predict(fisher1)
# Running PCA on fisher's dataset then printing the analysis

plot(fisher1)
# Using the summary to get a more undestandable insight into the data
summary(fisher1)
fisher1$rotation
# Plotting the Eigenvalues of the fisher dataset by how much of the variance they explain
plot(fisher1, main = "Fisher's Iris Dataset", col = "blue")
fisher_var_explain <- (fisher1$sdev^2) / (sum(fisher1$sdev^2))
plot(fisher_var_explain, type = "b", main = "Fisher's Iris Data",
xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:4)
# Running the same analysis on a different dataset for practice
oliveOil  <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
dim(oliveOil)
head(oliveOil, n = 5)
olive1 <- prcomp(oliveOil[,3:10], scale = TRUE)
summary(olive1)
diag(cov(oliveOil[,3:10]))
olive_var_explain <- (olive1$sdev^2) / (sum(olive1$sdev^2))
plot(olive_var_explain, type = "b", main = "Olive Oil Data",
xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n", col = "blue")
axis(1, at = 3:10)
# Seeing what variables are useful for breaking up the data
pairs(iris[,c(1:4)], col=iris[,5])
# Using the Prcomp to see what commmbinations of the principa components are useful
pairs(newiris[,c(1:4)], col=iris[,5])
# plotting the principal components on their own against each other
plot(newiris[,1], newiris[,2], type="n", xlab="PC1", ylab="PC2")
text(newiris[,1], newiris[,2], labels=substr(iris[,5],1,2), col=as.integer(iris[,5]))
# Checking if the prcomp still works with scaled variables
fisher2 <- prcomp(iris[,c(1:4)], scale= TRUE)
newiris2 <- predict(fisher2)
pairs(newiris2[,c(1:4)], col = iris[,5])
plot(newiris2[,1], newiris2[,2], type="n", xlab="PC1", ylab="PC2")
text(newiris2[,1], newiris2[,2], labels=substr(iris[,5],1,2), col=as.integer(iris[,5]))




