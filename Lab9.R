
# Title     : Lab 9
# Objective : Multidimensional Data Scaling
# Created by: ThomasGrant
# Created on: 03/12/2020
library("MASS")
oliveOil  <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
N <- sample(1:323, size = 30)
D <- sample(324:421, size = 30)
S <- sample(422:572, size = 30)
acidsamp <- oliveOil[c(N, D, S), 3:10]
?cmdscale
acid_sample_dist <- dist(acidsamp, method = "euclidean")
loc1 <- cmdscale(acid_sample_dist, k=3, eig = TRUE)
x <- loc1$points[,1]
y <- loc1$points[,2]
cols <- c(rep(1,30),rep(2,30),rep(3,30))
plot(x, y, type = "n", xlab = "", ylab ="", main = "Classical")
text(x, y, oliveOil[c(N, D, S), 1], cex = 1, col = cols)
loc_total <- cmdscale(dist(acidsamp), k = (nrow(acidsamp)-1), eig=TRUE)
sum(abs(loc_total$eig[1:2])) / sum(abs(loc_total$eig)) #  Computing the coverage of the first two eigen vectors.
loc2 <- sammon(acid_sample_dist, k=3)
loc3 <- isoMDS(acid_sample_dist, k=3)
install.packages("vegan")
library('vegan')
proc12 <- procrustes(loc1$points,loc2$points) # Running procrustes analysis on the different MDS methods
proc23 <- procrustes(loc2$points,loc3$points)
proc13 <- procrustes(loc3$points,loc1$points)
# Visualising the proc
plot(proc12)
plot(proc13, kind = 2)
# Exercise 1: Show the sum of square errors for each procrustes comparison
proc12$ss
proc13$ss
proc23$ss
# Run your analysis on standardised data for the acids
acid_sd <- apply(oliveOil[,3:10], 2 ,sd)
standard_acids <-sweep(oliveOil[,3:10], 2, acid_sd, "/")
stand_acid_samp <- standard_cids[c(N, D, S),]




