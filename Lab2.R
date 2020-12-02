# Title     : Lab 2
# Objective : Learn how to subset data, implement simple linear algebra routines
#   and plot and analyse data
# Created by: ThomasGrant
# Created on: 26/10/2020
install.packages("mclust")
install.packages("cluster")
music <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/music.csv")
head(music, 10)
dim(music)
music[c(2,5),c(1,3,4)]
M2 <- music[c(1,3,4), c(2:5)]
music_num <- music[, c(4:8)]
cov(music_num)
cor(music_num)
A <- matrix(c(1,2,2,5), nrow = 2, ncol = 2, byrow = TRUE)
A
res <- eigen(A)
res$vectors
res$values
Av <- A %*% res$vectors[,1]
LambdaV <- res$values[1] * res$vectors[,1]
Av
LambdaV
frogCranialM <- matrix(c(17.7,20.3,20.3,24.4), nrow = 2,ncol = 2,byrow = TRUE)
frogEigen <- eigen(frogCranialM)
frogEigen
