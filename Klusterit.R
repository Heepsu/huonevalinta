#
#
#
# Cluster-analysis for a part of the data, replication script. Commented.
#

library(pheatmap)
library(factoextra)
# Install the package (run this once)
install.packages("readxl")

# Load the package
library(readxl)

Tunnevastaukse <- read_excel("/Users/heidi/Documents/GitHub/huonevalinta/Tunnevastaukse.xlsx")


# 1. Prepare the data
data_matrix <- as.matrix(Tunnevastaukse[,-1])  # Remove first column (names)
rownames(data_matrix) <- Tunnevastaukse[,1]$VideoID    # Use names as row labels

# 2. Compute distance matrix
distance_matrix <- dist(data_matrix, method = "euclidean")


# 3. Heatmap of distances
pheatmap(as.matrix(distance_matrix),
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         main = "Distance Heatmap of Observations")


# 4. PCA analysis
pca <- prcomp(data_matrix, scale. = TRUE)

# 4.1 Save PCA Loadings
# The loadings (rotation) show the weight of each variable on each PC.
pca_loadings <- as.data.frame(pca$rotation)
pca_loadings$Variable <- rownames(pca_loadings) # Add variable names as a column

# Reorder columns to put the variable name first
pca_loadings <- pca_loadings[, c(ncol(pca_loadings), 1:(ncol(pca_loadings) - 1))]

cat("\nPCA Loadings (How Emotions contribute to each PC):\n")
print(pca_loadings)

# Save the loadings to a CSV file (optional, but good practice)
write.csv(pca_loadings, "PCA_Loadings_Emotion_Ratings.csv", row.names = FALSE)

# 4.2 Explained Variance 🔥 NEW CODE ADDED HERE 🔥
pca_variance <- pca$sdev^2
proportion_variance_explained <- pca_variance / sum(pca_variance)
cumulative_variance_explained <- cumsum(proportion_variance_explained)

variance_summary <- data.frame(
  PC = 1:length(pca_variance),
  Variance = pca_variance,
  Proportion_Explained = proportion_variance_explained,
  Cumulative_Explained = cumulative_variance_explained
)

cat("\nExplained Variance by Principal Component:\n")
print(variance_summary)

write.csv(variance_summary, "PCA_Explained_Variance.csv", row.names = FALSE)

# 5. Hierarchical clustering
hc <- hclust(distance_matrix)
plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "", sub = "")



# 6. Elbow method for optimal clusters
fviz_nbclust(data_matrix, kmeans, method = "wss") +
  ggtitle("Elbow Method for Optimal Number of Clusters")



# 7. Automatic Outlier Detection using Mahalanobis distance
center <- colMeans(data_matrix)
cov_matrix <- cov(data_matrix)
mahal_dist <- mahalanobis(data_matrix, center, cov_matrix)

# Threshold for outliers (Chi-square with df = number of variables)
threshold <- qchisq(0.975, df = ncol(data_matrix))  # 97.5% confidence
outliers <- which(mahal_dist > threshold)

cat("Potential outliers:\n")
print(rownames(data_matrix)[outliers])

# Bar chart of Mahalanobis distances
barplot(mahal_dist,
        names.arg = rownames(data_matrix),
        las = 2,
        col = ifelse(mahal_dist > threshold, "red", "gray"),
        main = "Mahalanobis Distances (Outliers in Red)",
        ylab = "Distance")

abline(h = threshold, col = "blue", lwd = 2, lty = 2)



# 8. PCA plot with cluster colors
clusters <- cutree(hc, k = 2)  # Adjust k for number of clusters
plot(pca$x[,1:2],
     col = clusters, pch = 19,
     xlab = "PC1", ylab = "PC2",
     main = "PCA with Cluster Colors")
text(pca$x[,1:2], labels = rownames(data_matrix), pos = 3, cex = 0.8)



# Highlight outliers in PCA plot
points(pca$x[outliers,1], pca$x[outliers,2], col = "red", pch = 8, cex = 3.5)



# 9. Summary table: observation, cluster, outlier flag
summary_table <- data.frame(
  Observation = rownames(data_matrix),
  Cluster = clusters,
  Outlier = ifelse(rownames(data_matrix) %in% rownames(data_matrix)[outliers], "Yes", "No")
)



# 10. ANOVA to test differences between clusters
# Reshape data for ANOVA
df_long <- data.frame(
  Observation = rep(rownames(data_matrix), times = ncol(data_matrix)),
  Cluster = rep(clusters, times = ncol(data_matrix)),
  Variable = rep(colnames(data_matrix), each = nrow(data_matrix)),
  Value = as.vector(data_matrix)
)

anova_result <- aov(Value ~ Cluster, data = df_long)
cat("\nANOVA Results:\n")
print(summary(anova_result))


