# Title     : Lab 4
# Objective : Write code for various dissimilarity computation methods
# and linkage methods for hierarchical clustering
# Created by: ThomasGrant
# Created on: 29/10/2020

#Import the Olive Oil Dataset
OliveOil <- read.csv('https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv')
# Check the first 10 entries into the olive oil dataset to see the layou of the data
head(OliveOil, n=10)
# Check how many entries there are for the olive oil dataset
dim(OliveOil)
head(OliveOil, n=6)
# Exercise1 Create a matrix containing the last 8 columns of the Olive Oil dataset
Acids <- OliveOil[,3:10]
dim(Acids)
head(Acids, n = 6)
# Check the help file for dist, which is used for calculating  dissimilarity
?dist
# Calculate the dissimilarity of the Acid data and format it as a matrix
acids_dis <- dist(Acids, method="euclidean")
acids_dis_mat <- as.matrix(acids_dis)
# Find the dissimilaroty of entries 1:10
acids_dis_mat[c(1:10),c(1:10)]
# Compare this to the dissimilaroty calculated using the Manhattan method
#Exercise 1
manhattan_acids_dis <- dist(Acids, method="manhattan")
manhattan_acids_dis_mat <- as.matrix(manhattan_acids_dis)
manhattan_acids_dis_mat[c(1:10), c(1:10)]
# Create new dissimilarity df which includes the first 3 columns of OliveOil
olive_dis <- dist(OliveOil, method = "euclidean")
olive_dis_mat <- as.matrix(olive_dis)
# Use the row.names operator to specify what you want the firsy 5 elements of.
olive_dis_mat[c(1:5, row.names("Inland Sardinia")), c(1:5, row.names("Umbria") )]
# Exercise 2
# Create Dendrogram using manhattan method for calculating dissimilarity
# and single linkage
#
manhattanClust <- hclust(manhattan_acids_dis, method = "average")
#plot(manhattanClust)
# You can see the clusters more easily using the single method of conecting
# but it is harder to understand how the different clusters merge
# Exercise 3
# Use clust1$merge to calculate the recommended cut off height
?hclust
h_mean <- mean(manhattanClust$height)
sd_h <- sd(manhattanClust$height)
h_cut_off <- h_mean + 3*sd_h
# dendrogram
plot(manhattanClust)
# This line shows where the merging should stop
abline(h = h_cut_off, lty=2, col=2)
# Decides the number of clusters the data is broken into
acids_label1 <- cutree(manhattanClust, k=10)
# Decides the height after which they stop clustering
acids_label2 <- cutree(manhattanClust, h=h_cut_off)
# Plot the first and second variables in the Acid dataset using diferrent colours
palette(rainbow(10))
plot(Acids[,1], Acids[,2], col = acids_label1)
# Pair the Acids variables against each other and plot all of them using the pairs command
pairs(Acids, col = acids_label1)
# The 2 in the apply function refers to the fact that the data is being analysed columnally.
# if you change it to 1 it would work by row
acid_sd <- apply(Acids, 2 ,sd)
acid_sd
acid_mean <- apply(Acids, 2, mean)
acid_mean
# Use the sweep function to standardise the Acids database
standard_acids <- sweep(Acids, 2, acid_sd, "/")
standard_acids_mean <- sweep(Acids, 2, acid_mean, "-")
# Perform cluster analysis of the standardised Acid daa
standard_acids_dis <- dist(standard_acids, method="manhattan")
standard_Acids_Clust <- hclust(standard_acids_dis, method = "average")
h_mean2 <- mean(standard_Acids_Clust$height)
sd_h2 <- sd(standard_Acids_Clust$height)
h_cut_off2 <- h_mean2 + 3*sd_h2
plot(standard_Acids_Clust)
abline(h = h_cut_off2, lty=2, col=2)
acids_label3 <- cutree(standard_Acids_Clust, h=h_cut_off2)
pairs(standard_acids, col = acids_label3)
# Perform Cluster analysis of old faithful data
#?faithful
faithful_dis <- dist(faithful, method = "manhattan")
faithful_clust <- hclust(faithful_dis, method = "complete")
faith_mean <- mean(faithful_clust$height)
faith_sd <- sd(faithful_clust$height)
faith_cutOff <- faith_mean + (faith_sd*3)
plot(faithful_clust)
abline(h = faith_cutOff, lty = 2, col = 2)
faithful_label <- cutree(faithful_clust, h = faith_cutOff)
pairs(faithful, col = faithful_label)
