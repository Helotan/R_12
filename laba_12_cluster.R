# Load required libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(knitr)

# Load the data
data <- read.csv("missile_attacks_daily.csv", header = TRUE, sep = ",")

# Convert time_start to Date type
data$time_start <- as.Date(data$time_start)

# Filter data for the same period (30 days) but for the year 2023
start_date <- as.Date("2023-01-01")
end_date <- start_date + 29
data_2023 <- subset(data, time_start >= start_date & time_start <= end_date & !is.na(launched))

# Prepare the data for clustering
clustering_data <- data_2023 %>%
  select(launched, destroyed) %>%
  na.omit()

# Scale the data
scaled_data <- scale(clustering_data)

# Create and display object-property table
object_property_table <- clustering_data
rownames(object_property_table) <- paste("Attack", 1:nrow(clustering_data))
print("Object-Property Table:")
kable(head(object_property_table))

# Compare different distance metrics
metrics <- c("euclidean", "manhattan", "maximum")
dist_matrices <- list()
for(metric in metrics) {
  dist_matrices[[metric]] <- dist(scaled_data, method = metric)
  cat("\nFirst 5x5 proximity matrix using", metric, "distance:\n")
  print(as.matrix(dist_matrices[[metric]])[1:5, 1:5])
}

# Create complete proximity matrix using selected metric (euclidean)
proximity_matrix <- as.matrix(dist_matrices[["euclidean"]])
cat("\nFull Euclidean Proximity Matrix (first 5 rows/columns):\n")
print(proximity_matrix[1:5, 1:5])

# Perform agglomerative hierarchical clustering with different linkage methods
linkage_methods <- c("complete", "average", "single", "ward.D2")
hc_results <- list()

for(method in linkage_methods) {
  hc_results[[method]] <- hclust(dist_matrices[["euclidean"]], method = method)
}

# Plot dendrograms for each linkage method
par(mfrow=c(2,2))
for(method in linkage_methods) {
  plot(hc_results[[method]], 
       main = paste("Dendrogram using", method, "linkage"),
       xlab = "Observations",
       ylab = "Height",
       cex = 0.6)
}

# Calculate cophenetic correlation for each method
cat("\nCophenetic Correlation Coefficients:\n")
for(method in linkage_methods) {
  coph_corr <- cor(dist_matrices[["euclidean"]], 
                   cophenetic(hc_results[[method]]))
  cat(method, ":", round(coph_corr, 3), "\n")
}

# Select best method based on cophenetic correlation
best_method <- linkage_methods[which.max(sapply(hc_results, function(x) 
  cor(dist_matrices[["euclidean"]], cophenetic(x))))]

cat("\nBest clustering method based on cophenetic correlation:", best_method)

# Final enhanced dendrogram with best method
par(mfrow=c(1,1))
plot(hc_results[[best_method]],
     main = paste("Final Dendrogram using", best_method, "linkage"),
     xlab = "Missile Attacks",
     ylab = "Distance",
     hang = -1,
     cex = 0.6)
rect.hclust(hc_results[[best_method]], k=3, border="red")