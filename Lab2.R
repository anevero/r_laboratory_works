# Read the data from the file.
current_data <- read.table(file = "input_files/input_lab2.txt")
current_data

# Convert the data to the vector.
data_vector <- current_data[,1]
data_vector

# Print the average value (среднее значение).
mean(data_vector)

# Print dispersion (дисперсия).
var(data_vector)

# Print standard deviation (среднее квадратичное отклонение).
sd(data_vector)

# Print mode (the most often value in the table, created from the vector) and
# its frequency (мода).
which.max(table(data_vector))

# Print median (медиана).
median(data_vector)

# Print asymmetry coefficient (коэффициент ассиметрии, вычислен по формуле).
sum(((sort(unique(data_vector))) - mean(data_vector)) ^ 3 * table(data_vector) /
      (length(data_vector) * sd(data_vector) ^ 3))

# Print excess ratio (коэффициент эксцесса, вычислен по формуле).
sum(((sort(unique(data_vector))) - mean(data_vector)) ^ 4 * table(data_vector) /
      (length(data_vector) * sd(data_vector) ^ 4)) - 3

library(fBasics)
# Print asymmetry coefficient (коэффициент ассиметрии, вычислен библиотечной
# функцией).
skewness(data_vector)
# Print excess ratio (коэффициент эксцесса, вычислен библиотечной функцией).
kurtosis(data_vector)

# Print truncated mean (k = 0.1) (усеченное среднее, отсекается по 10% 
# значений справа и слева).
mean(data_vector, trim = 0.1, na.rm = TRUE)

# Print the coefficient of variation (коэффициент вариации).
sd(data_vector) / mean(data_vector)

# Print relative linear deviation (относительное линейное отклонение).
sum(abs(data_vector - mean(data_vector))) / 
  (length(data_vector) * mean(data_vector))
