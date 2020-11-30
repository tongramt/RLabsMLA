# Title     : Lab 1
# Objective : Write and annotate code for lab 1
# Created by: ThomasGrant
# Created on: 26/10/2020
# Using the help function in r
?exp
# using functions such as logs and storing variables
h <- log(10, base = 10)
i <- log(10, base = 2)
y <- exp(2.5)
y
# Setting up matrices in r
a <- matrix(c(1,2,3,4),nrow = 2, ncol = 2, byrow = TRUE,)
a
b <- matrix(c(1,2,2,3,4,5), nrow = 3, ncol = 2, byrow = TRUE)
b
# Installing packages in r
install.packages("cluster")
install.packages("mclust")
# Downloading datasets in r
music <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/music.csv")
# displaying data from datasets
head(music, 10)
