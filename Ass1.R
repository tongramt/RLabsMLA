# Title     : Assignment 1
# Objective :
# Created by: ThomasGrant
# Created on: 03/12/2020
install.packages("factoextra")
library("factoextra")
library('flexclust')
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
euclid_dis <- dist(heart, method = "euclidean")
manhattan_dis <- dist(heart, method = "manhattan")
eucl_av_clust <- hclust(euclid_dis, method = "average")
eucl_comp_clust <- hclust(euclid_dis, method = "complete")
man_av_clust <-  hclust(manhattan_dis, method = "average")
man_comp_clust <- hclust(manhattan_dis, method = "complete")
# using these cutoffs to analyse clusts
plot(eucl_av_clust)
plot(eucl_comp_clust)
plot(man_av_clust)
plot(man_comp_clust)

# Using supervised learning Methods
# 1. K-Nearest Neighbours

# 2. LDA
# 3. QDA
# 4. K-Means Testing

