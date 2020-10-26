# Title     : TODO
# Objective : TODO
# Created by: ThomasGrant
# Created on: 26/10/2020
?iris
dim(iris)
head(iris,n = 5)
fisher1 <- prcomp(iris[,1:4], scale = TRUE)
fisher1
summary(fisher1)
plot(fisher1, main = "Fisher's Iris Dataset")
fisher_var_explain <- (fisher1$sdev^2) / (sum(fisher1$sdev^2))
plot(fisher_var_explain, type = "b", main = "Fisher's Iris Data",
xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:4)
oliveOil  <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
dim(oliveOil)
head(oliveOil, n = 5)
olive1 <- prcomp(oliveOil[,3:10])
summary(olive1)
diag(cov(oliveOil[,3:10]))
olive1 <- prcomp(oliveOil[,3:10], scale = TRUE)
summary(olive1)
diag(cov(oliveOil[,3:10]))