library(ggplot2)
library(dplyr)

# Perform PCA
pca_result <- prcomp(Walmart_sales[, -c(1:2)], scale. = TRUE)

# Scree plot
screeplot(pca_result, type = "line", main = "Scree Plot of PCA")

# Extract PC scores
pc_scores <- as.data.frame(pca_result$x[, 1:2])

pc_scores

# Extract loadings
loadings <- pca_result$rotation[, 1:2]

# Set a threshold for significant loadings
threshold <- 0.5

# Find variables with high loadings on PC1
variables_pc1 <- rownames(loadings)[abs(loadings[, 1]) > threshold]

# Find variables with high loadings on PC2
variables_pc2 <- rownames(loadings)[abs(loadings[, 2]) > threshold]

# Combine variables from PC1 and PC2
selected_variables <- union(variables_pc1, variables_pc2)

# Print selected variables
print(selected_variables)

# Scatter plot of PC scores
plot(pc_scores$PC1, pc_scores$PC2, xlab = "PC1", ylab = "PC2", main = "Scatter plot of PC1 vs PC2")

ggplot(pc_scores, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "Scatter Plot of PC Scores (PC1 vs. PC2)",
       x = "PC1",
       y = "PC2") +
  theme_minimal()

# Perform K-means clustering
kmeans_result <- kmeans(pc_scores[, c("PC1", "PC2")], centers = 3)

# Visualize the clusters
plot(pc_scores$PC1, pc_scores$PC2, xlab = "PC1", ylab = "PC2", main = "Scatter plot of PC1 vs PC2")
points(kmeans_result$centers, col = 2:4, pch = 19)  # Plot cluster centers
for (i in 1:length(kmeans_result$centers[, 1])) {
  segments(kmeans_result$centers[i, 1], kmeans_result$centers[i, 2],
           pc_scores$PC1[kmeans_result$cluster == i],
           pc_scores$PC2[kmeans_result$cluster == i],
           col = adjustcolor(i + 1, alpha.f = 0.2))
}
legend("topright", legend = paste("Cluster", 1:3), col = 2:4, pch = 19)

# Assess cluster membership
table(kmeans_result$cluster)
pc_scores_with_clusters <- cbind(pc_scores, Cluster = kmeans_result$cluster)
pc_scores_with_clusters

# Plot PC1 vs PC2 with clusters
plot(pc_scores_with_clusters$PC1, pc_scores_with_clusters$PC2, 
     col = pc_scores_with_clusters$Cluster, 
     pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "Scatter plot of PC1 vs PC2 with Clusters")

# Add legend
legend("topright", legend = unique(pc_scores_with_clusters$Cluster),
       col = unique(pc_scores_with_clusters$Cluster), pch = 19)
