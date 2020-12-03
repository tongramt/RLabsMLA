# Title     : Lab 8
# Objective : K-Means Clustering
# Created by: ThomasGrant
# Created on: 03/12/2020
library("MASS")
?faithful
# plot the old faithful datset
plot(faithful)
# Create a vector Within Sum of squares WSS which for computing the wss of the different values of K
WSS <- rep(0,10)
for(i in 1:10){
 WSS[i] <- sum(kmeans(faithful, centers = i)$withinss)
}
# Plot the WSS to see the best value for k. look for a kink in the line
plot(WSS, type = "l", xlab = "no. of clusters")
# investigate the best value of k
k <- 2
cl2 <- kmeans(faithful,centers = k)
table(cl2$cluster)
cl2$centers
cl2$withinss
plot(faithful, col = cl2$cluster)
points(cl2$centers, col=1:k, pch=8, cex=5) # shows the centroids of the clusters
# Exercise calculate the average distance to the centroid for the 2 clusters
avg_dst <- function(dataset, no_of_clusters){
   avg_dist <- rep(0,no_of_clusters)
   clk <- kmeans(dataset, centers = no_of_clusters)
  for(i in 1:no_of_clusters){
    g1 <- dataset[which(clk$cluster==i),]
    ng <- clk$size[i]
    total <- sum(as.matrix(dist(rbind(g1, clk$centers[i,])))[ng+1,])
    avg_dist[i] <- total/ng
  }
  avg_dist
}
avg_dst(faithful, 3)
