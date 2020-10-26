# Title     : TODO
# Objective : TODO
# Created by: ThomasGrant
# Created on: 26/10/2020
music <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/music.csv")
head(music, 10)
dim(music)
music[c(2,5),c(1,3,4)]
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
