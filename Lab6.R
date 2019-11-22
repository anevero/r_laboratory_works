library(MASS)

# Save the number of all the pairs of values.
number_of_values <- 30

# Save the number of letters in the surname (it will be used later, according 
# to the task).
generation_constant <- 6

# Generate first 20 pairs of values.
x1 <- rnorm(20, mean = 0, sd = generation_constant %/% 3)
y1 <- rnorm(20, mean = 0, sd = generation_constant %/% 3)

# Generate second 10 pairs of values.
x2 <- rnorm(10, mean = generation_constant, sd = generation_constant %/% 3)
y2 <- rnorm(10, mean = generation_constant, sd = generation_constant %/% 3)

# Bind two-dimensional data.
data <- cbind(c(x1, x2), c(y1, y2))

# Divide the data into two clusters.
two_clusters <- kmeans(data, 2)

# Save clusters data.
clusters_data <- two_clusters$cluster

# Save the number of training and testing pairs.
number_of_training <- floor(30 * 0.7)
number_of_testing <- number_of_values - number_of_training

# ----------------------------------------------------------------------------
# The first task.

# Create training and testing data subsets.

# Choose random indexes (which will be the indexes of the training set of 
# the data).
training_indexes <- sample(1:number_of_values, number_of_training)
# Choose the remaining indexes (which will be the indexes of the testing set of
# the data).
testing_indexes <- 
  (1:number_of_values)[!(1:number_of_values %in% training_indexes)]

# Save the information about the real clusters of the training and testing data.
training_clusters <- clusters_data[training_indexes]
testing_clusters <- clusters_data[testing_indexes]

# Training process.
model <- qda(data[training_indexes,], training_clusters) 
trained_testing_clusters <- predict(model, data[testing_indexes,])$class

# Analyze the classification error.
sum(trained_testing_clusters != testing_clusters) / number_of_testing

# Create the plot.
incorrect_indexes <- 
  testing_indexes[trained_testing_clusters != testing_clusters]
plot(data, type = "n", xlab = "X", ylab = "Y")

# Add different points to the plot. Points, which were used as training, will
# be shown with pch = 2, points, which were used as testing, will be shown with
# pch = 1. Colors will be chosen according to the clusters. If the point's
# cluster is incorrect (trained cluster doesn't match the real cluster), the
# red color will be assigned to it.

points(data[training_indexes,], 
       col = ifelse(training_clusters == 1, "blue", "green"), pch = 2)
points(data[testing_indexes,],
       col = ifelse(testing_clusters == 1, "blue", "green"), pch = 1)

points(data[incorrect_indexes,], col = "red")

# ----------------------------------------------------------------------------
# The second task.

# Choose indexes of the elements, whose clusters will be changed.
changing_indexes <- sample(1:number_of_values, floor(0.2 * number_of_values))

# Change the cluster information.
for (i in changing_indexes) {
  clusters_data[i] <- (clusters_data[i] + 1) %% 2
}

# Just the same process as above.

training_indexes <- sample(1:number_of_values, number_of_training)
testing_indexes <- 
  (1:number_of_values)[!(1:number_of_values %in% training_indexes)]

training_clusters <- clusters_data[training_indexes]
testing_clusters <- clusters_data[testing_indexes]

model <- qda(data[training_indexes,], training_clusters) 
trained_testing_clusters <- predict(model, data[testing_indexes,])$class

sum(trained_testing_clusters != testing_clusters) / number_of_testing

incorrect_indexes <- 
  testing_indexes[trained_testing_clusters != testing_clusters]
plot(data, type = "n", xlab = "X", ylab = "Y")

points(data[training_indexes,], 
       col = ifelse(training_clusters == 1, "blue", "green"), pch = 2)
points(data[testing_indexes,],
       col = ifelse(testing_clusters == 1, "blue", "green"), pch = 1)

# Incorrect points.
points(data[incorrect_indexes,], col = "red")
# Points with incorrect (changed) cluster.
points(data[changing_indexes,], pch = 3)
