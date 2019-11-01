# Set output precision.
options(digits=5)

# Read the data from the file.
current_data <- read.table(file = "input_files/input_lab4.txt")

# Save the length of the series.
series_length <- length(current_data[,2])

# Save and print the average level of the series. 
chrono_average <- sum(current_data[,2])
chrono_average <- chrono_average - (current_data[1, 2] / 2)
chrono_average <- 
  chrono_average - (current_data[series_length, 2] / 2)
chrono_average <- chrono_average / (series_length - 1)
chrono_average

# Save the average value of the series.
series_mean <- mean(current_data[,2])

# ----------------------------------------------------------------------------
# Working the series criteria.

# Create the copy of the table. Then the information about the types of the
# values will be added to this new table.
series_table <- current_data

# Add the information about the type of the value as the third column of
# the table.
series_table[1:series_length, 3] <- 
  ifelse(series_table[1:series_length, 2] < series_mean, 1, 0)

# Count the number of series.
number_of_series <- length(rle(series_table[,3])$lengths)

# Specify t coefficient (corresponding to the confidence level p = 0.95).
t_coefficient = 1.96

# Print the result of applying series criteria.
average_number_of_series = (series_length + 1) / 2
number_of_series_sd = sqrt(series_length - 1) / 2

if (number_of_series >= 
    average_number_of_series - t_coefficient * number_of_series_sd &
    number_of_series <= 
    average_number_of_series + t_coefficient * number_of_series_sd) {
  print("No regularity has been found.")
} else {
  print("Regularity has been found.")
}

# ----------------------------------------------------------------------------
# Working with the smoothing.

# Use three-level moving average method. Put necessary values to the
# third column of the original table.
current_data[1, 3] <- current_data[1, 2]
current_data[series_length, 3] <- current_data[series_length, 2]

for (i in 2:(series_length - 1)) {
  current_data[i, 3] = 
    mean(c(current_data[i - 1, 2], current_data[i, 2], current_data[i + 1, 2]))
}

# Use linear analytical alignment method. Put necessary values to the
# fourth column of the original table.

# Move the system of the coordinates (change the values in the first column of
# the table).
if (series_length %% 2 == 0) {
  for (i in (series_length / 2):1) {
    current_data[i, 1] = -(2 * (series_length / 2 - i) + 1)
    current_data[series_length - i + 1, 1] = 2 * (series_length / 2 - i) + 1
  }
} else {
  current_data[floor(series_length / 2) + 1, 1] = 0
  for (i in (floor(series_length / 2)):1) {
    current_data[i, 1] = -(floor(series_length / 2) - i + 1)
    current_data[series_length - i + 1, 1] = floor(series_length / 2) - i + 1
  }
}

# Count linear coefficients.
a0_coefficient <- chrono_average
a1_coefficient <- sum(current_data[,1] * current_data[,2])
a1_coefficient <- a1_coefficient / sum(current_data[,1]^2)

# Fill the table.
current_data[1:series_length, 4] <-
  a0_coefficient + a1_coefficient * current_data[1:series_length, 1]

# ----------------------------------------------------------------------------
# Working with the plot.

plot(current_data[,2], type = "l", col = "black",
     xlab = "T", ylab = "Y", main = "График")
lines(current_data[,3], type = "l", col = "blue")
lines(current_data[,4], type = "l", col = "red")
