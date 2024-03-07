library(cluster)
library(readr)
library(factoextra)
library(magrittr)
library(NbClust)
library(ggplot2)

Walmart_sales <- read.csv("D:/MVA/Git/dataset/Walmart_sales.csv")

# hierarchical clustering
hc <- hclust(dist(scale(Walmart_sales[, -c(1:2)])), method = "complete")

# dendrogram
plot(hc, main = "Dendrogram for Hierarchical Clustering")

hc_clusters <- cutree(hc, k = 3)

# membership for each cluster
table(hc_clusters)

# K-means clustering
kmeans_result <- kmeans(scale(Walmart_sales[, -c(1:2)]), centers = 3)

# membership for each cluster
table(kmeans_result$cluster)

# clusters using first two Principal Components
pca_result <- prcomp(Walmart_sales[, -c(1:2)], scale. = TRUE)
pc_scores <- as.data.frame(pca_result$x[, 1:2])
pc_scores_with_clusters <- cbind(pc_scores, Cluster = kmeans_result$cluster)

# Scatter plot of PC scores
ggplot(pc_scores_with_clusters, aes(x = PC1, y = PC2, color = factor(Cluster))) +
  geom_point() +
  labs(title = "Clusters and Membership Visualization (PC1 vs PC2)",
       x = "PC1",
       y = "PC2",
       color = "Cluster") +
  theme_minimal()
