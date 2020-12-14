# Title     : Assignment 1
# Objective :
# Created by: ThomasGrant
# Created on: 03/12/2020
install.packages("factoextra")
library("factoextra")
library('flexclust')
library("MASS")
library("ellipse")
heart <- read.csv("/Users/ThomasGrant/Downloads/heart_data(1).csv")
# In the description of the data it says that the values for column 9 are between 0 and 3
# and in the dataset they are between 1 and 4
for(i in 1:nrow(heart)){
  heart[i,9] <- heart[i,9]-1
}
as.integer(heart[,9])
heart <- heart[order(heart$Class),]

# Analyse what components provide us with good insight into the data
# Using unsupervised learning methods 30%
# 1: Principal Component Analysis
head(heart, n=5)
# Break Heart data down to real variables only
real_heart <- heart[,c(1,3,4,6,8,9)]
real_pca <- prcomp(real_heart, scale=TRUE)
summary(real_pca)

# Graph to show the proportion of variance explained
eig_summary <- get_eigenvalue(real_pca)
eig_plot <- fviz_eig(real_pca, addlabels = TRUE, ylim = c(0, 60), barcolor = "red", barfill = "red", main = "Heart Disease PCA")
# Group pca by class
fviz_pca_biplot(real_pca,
                col.ind = heart$Class, palette = "jco",
                legend.title = "Heart Disease",
                geom.ind = "point", col.var = "red")

# Cannot add ellipses. PCA shows class is afffected by dim 2
# 2: Hierarchical Clustering
control_dis <- dist(heart[,10], method = "euclidean")
?hclust
control_clust <- hclust(control_dis, method = "single")
euclid_dis <- dist(heart, method = "euclidean")
manhattan_dis <- dist(heart, method = "manhattan")
eucl_av_clust <- hclust(euclid_dis, method = "average")
eucl_comp_clust <- hclust(euclid_dis, method = "complete")
man_av_clust <-  hclust(manhattan_dis, method = "average")
man_comp_clust <- hclust(manhattan_dis, method = "complete")
cutoff <- function(clust){
  h_mean <- mean(clust$height)
sd_h <- sd(clust$height)
h_cut_off <- h_mean + 3*sd_h
  return <- h_cut_off
}
eucl_av_h <- cutoff(eucl_av_clust)
eucl_comp_h <- cutoff(eucl_comp_clust)
man_av_h <- cutoff(man_av_clust)
man_comp_h <- cutoff(man_comp_clust)
# using these cutoffs to analyse clusts
clustPlot <- function(clust, h_cut_off){
  plot(clust)
abline(h = h_cut_off, lty=2, col=2)
}
clustPlot(eucl_av_clust,eucl_av_h)
clustPlot(eucl_comp_clust, eucl_comp_h)
clustPlot(man_av_clust, man_av_h)
clustPlot(man_comp_clust, man_comp_h)
control_label <- cutree(control_clust, k=2)
eucl_av_lab <- cutree(eucl_av_clust, k = 3)
eucl_comp_lab <- cutree(eucl_comp_clust, k = 5)
man_av_lab <- cutree(man_av_clust, k = 5)
man_comp_lab <- cutree(man_comp_clust, k = 5)
eucl_av_adjRI <- randIndex(control_label, eucl_av_lab)
eucl_comp_adjRI <- randIndex(control_label, eucl_av_lab)
man_av_adjRI <- randIndex(control_label, eucl_av_lab)
man_comp_adjRI <- randIndex(control_label, eucl_av_lab)
eucl_av_adjRI
eucl_comp_adjRI
man_av_adjRI
man_comp_adjRI
# None of the HC's provided insight into the data
?randIndex
# 4. K-Means Testing
WSS <- rep(0,12)
for(i in 1:10){
 WSS[i] <- sum(kmeans(real_heart, centers = i)$withinss)
}
plot(WSS, type = "l", xlab = "no. of clusters")
for(i in 1:4){
  for(j in 1:4){
    data <- c(x=real_heart[,i],y=real_heart[,j])
    for(k in 2:4){

      cl2 <- kmeans(data,centers = k)
      table(cl2$cluster)
      cl2$centers
      cl2$withinss
      plot(real_heart, col = cl2$cluster)
      points(cl2$centers, col=1:k, pch=8, cex=5)
  }}}# shows the centroids of the clusters

# Using supervised learning Methods
# 1. K-Nearest Neighbours
index_train <- c(1:50, 151:190)
index_test <- c(51:100,191:230)
index_valid <- c(101:150,231:270)
train <- heart[index_train, 1:9]
test <- heart[index_test, 1:9]
valid <- heart[index_valid, 1:9]
library(class)
?svm
#
kmax <- 150 # Max number of nearest neighbours
k <- 1:kmax # setting k as a vector of length=kmax
p <- rep(0, kmax)
ntest <- nrow(test) # number of rows in test
k_summary <- cbind(k, p)
colnames(k_summary) <- c("k","% misclassified")
for(i in 1:kmax){
result <- knn(train, test, cl = heart[index_train, 10], k = i)
class_agree <- table(result, heart[index_train,10])
sum_agree <- sum(diag(class_agree))
k_summary[i, 2] <- (ntest - sum_agree) / ntest
}
k_summary[1:25, ]
plot(k_summary, type="b", col = "blue")
which.min(k_summary[,2])



