# Title     : Lab 5
# Objective : Comparing different cluster solutions
# Created by: ThomasGrant
# Created on: 02/12/2020
require('cluster')
require('mclust')
install.packages('flexclust')
library('flexclust')
?clust
?mclust
# Perform hierarchical clustering for the olive oil data using euclidean
# and manhattan clustering methods and complete linkage
olive_Oil <- read.csv('https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv')
acids <- (olive_Oil[,3:10])
?dist
euclid_acids_dis<- dist(acids, method = "euclidean")
max_acids_dis <- dist(acids, method = "maximum")
euclid_acid_clust <- hclust(euclid_acids_dis, method = "complete")
max_acids_clust <- hclust(max_acids_dis,method = "complete")
# Create labels for the 2 clusters and conduct analysis of the agreement between the two labels.
euclid_label <- cutree(euclid_acid_clust, k = 4)
max_label <- cutree(max_acids_clust, k=4)
table(euclid_label, manhattan_label)
# calculate a Rand index to compare the two
normalRI <- randIndex(euclid_label, max_label, correct = FALSE)
adjRI <- randIndex(euclid_label, max_label)
normalRI
adjRI
# Conduct a 4 class clustering of the olive oil dataset using euclidean dissimilarity
# and average linkage and compare it to a 3 class clustering of the data using
# euclidean dissimilarity and complete linkage
euclid_acid_clust_avg <- hclust(euclid_acids_dis, method = "average")
euclid_label2 <- cutree(euclid_acid_clust, k = 3)
euclid_avg_label <- cutree(euclid_acid_clust_avg, k = 4)
normalRI2 <- randIndex(euclid_label2, euclid_avg_label, correct = FALSE)
adjRI2 <- randIndex(euclid_label2, euclid_avg_label)
normalRI2
adjRI2
# You may wish to do this as different dissimilarity methods may suggest different ammounts of clusters
# and you wish to see if they are the same clusters not joined yet
# Download the protein dataset and conduct hclust analysis on it
protein <- read.table("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/protein.txt")
head(protein, h=10)
dim(protein)
protein_euclid_dis <- dist(protein[2:26,2:10], method = "euclidean")
protein_manhattan_dis <- dist(protein[2:26,2:10], method = "manhattan")
protein_euclid_clust <- hclust(protein_euclid_dis, method = "complete")
protein_manhattan_clust <- hclust(protein_manhattan_dis, method="complete")
protein_manhattan_h_mean <- mean(protein_manhattan_clust$height)
protein_manhattan_h_sd <- sd(protein_manhattan_clust$height)
protein_euclid_h_mean <- mean(protein_euclid_clust$height)
protein_euclid_h_sd <- sd(protein_euclid_clust$height)
protein_manhattan_cutoff <- protein_manhattan_h_mean + (2*protein_manhattan_h_sd)
protein_euclid_cutoff <- protein_euclid_h_mean + (2*protein_euclid_h_sd)
plot(protein_euclid_clust)
abline(h=protein_euclid_cutoff)
plot(protein_manhattan_clust)
abline(h=protein_manhattan_cutoff)
# simulating data using R
sample <- rnorm(100000,0,1)
# Compute the mean and sd of the sample
mean(sample)
sd(sample)
# Generate a poisson distribution with n= 1, lambda=1
# calculate the mean and sd of this sample
sample_pois <- rpois(1000, 1)
mean(sample_pois)
sd(sample_pois)
# Generate a multivariate Normal distribution.
install.packages("MASS")
library("MASS")
mu <- c(7, 10)  # this vector is the mean vector of the distribution
sig <- matrix(c(2, 1, 1, 4), nrow = 2, ncol = 2) # this is the covariance matrix
multisim <- mvrnorm(100, mu, sig) # n specifies the number of observations
plot(multisim)
identify(multisim)
?identify
par(mfrow=c(1,1))
hist(multisim[, 2], freq = FALSE, breaks = 10)
curve(dnorm(x, mean=10, sd=2), add=TRUE)
curve( dnorm(x, mean = mean(multisim[,2]) , sd = sd(multisim[,2])), add = TRUE, lty = 2, col = 2)
boxplot(as.data.frame(multisim),notch=TRUE)
# How to create functions using R
square <- function(x){
x^2
}
square(3)
# Using for and if statements in r
for(i in 1:10){
  if(i==1){
    print(paste("The first number is", i))
  }
  if(i>1){
    print( paste("The next number is", i))
  }
}
# Create a function which takes a vector as its parameter and returns the
# sum of the elements if the no. elements is 10 or greater and the square of
# each element if its less than 10
exercise <- function(vector){
  vector_length <- length(vector)

  if(vector_length<10){
    result <- integer(vector_length)
    for(i in 1:vector_length){
      result[i] <- vector[i]^2
    }
  }
  if((vector_length>=10)){
    result <- 0
    for(i in 1:vector_length){
      result <- result+ vector[i]
    }
  }
  result
}
short_vector <- c(1,2,3,4,5)
long_vector <- c(1,2,3,4,5,6,7,8,9,10,11)
exercise(short_vector)
exercise(long_vector)

