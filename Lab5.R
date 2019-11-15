# Set output precision.
options(digits=5)

# Read the data from the file.
current_data <- read.table(file = "input_files/input_lab5.txt")

# Scale the data.
current_data[1] <- scale(current_data[1])
current_data[2] <- scale(current_data[2])

# ----------------------------------------------------------------------------
# Working with two clusters.

# Use K-means algorithm for dividing the data into two clusters.
two_clusters <- kmeans(current_data, 2)

# Print the number of points in the clusters.
table(two_clusters$cluster)

# Print the centers of the clusters.
two_clusters$centers

# Create two plots (dividing points by the color / form).
plot(current_data, col = ifelse(two_clusters$cluster == 1, "blue", "black"))
legend("topleft", legend = c("1", "2"), fill = c("blue", "black"))

plot(current_data, pch = ifelse(two_clusters$cluster == 1, 1, 2))
legend("topleft", legend = c("1", "2"), pch = c(1, 2))

# ----------------------------------------------------------------------------
# Working with three clusters.

# Use K-means algorithm for dividing the data into three clusters.
three_clusters <- kmeans(current_data, 3)

# Print the number of points in the clusters.
table(three_clusters$cluster)

# Print the centers of the clusters.
three_clusters$centers

# Create two plots (dividing points by the color / form).
plot(current_data, col = ifelse(three_clusters$cluster == 1, "blue",
                                ifelse(three_clusters$cluster == 2, "red",
                                       "black")))
legend("topleft", legend = c("1", "2", "3"), fill = c("blue", "red", "black"))

plot(current_data, pch = ifelse(three_clusters$cluster == 1, 1,
                                ifelse(three_clusters$cluster == 2, 2, 3)))
legend("topleft", legend = c("1", "2", "3"), pch = c(1, 2, 3))
