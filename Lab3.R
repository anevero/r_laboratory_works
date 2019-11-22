# Set output precision.
options(digits=4)

# Read the data from the file.
current_data <- read.table(file = "input_files/input_lab3.txt")

# Create vectors of x and y values.
x_vector <- current_data[,1]
y_vector <- current_data[,2]

# Count some x_vector characteristics.
x_vector_mean = mean(x_vector)
x_vector_sd = sd(x_vector)

# ----------------------------------------------------------------------------
# Working with the first table.

# Create the vector, which will become the base of the table.
first_table_base_vector <- rep(0, 15)
dim(first_table_base_vector) <- c(3, 5)

# Create an empty table.
first_table <- as.data.frame(first_table_base_vector)

# Fill the first and the second columns of the table with intervals information.
first_table[1:3, 1] <- x_vector_mean - 1:3 * x_vector_sd
first_table[1:3, 2] <- x_vector_mean + 1:3 * x_vector_sd

# Fill the third column of the table with the information about the number
# of items.
for (i in 1:3) {
  first_table[i, 3] <-
    length(which(x_vector >= first_table[i, 1] & x_vector < first_table[i, 2]))
}

# Fill the fourth column of the table with the relative values information.
first_table[1:3, 4] <- first_table[1:3, 3] / length(x_vector) * 100

# Fill the fifth column of the table with the predefined values.
first_table[1:3, 5] <- c(68.3, 95.4, 99.7)

# Set columns names.
colnames(first_table)<-c("Начало", "Конец", "Кол-во", "Кол-во в %",
                         "Норм. распределение")

first_table

# ----------------------------------------------------------------------------
# Working with the second table.

# Using Sturges' formula for counting the number of the groups.
number_of_groups <- 1 + floor(log(length(x_vector), 2))

# Create the vector, which will become the base of the table.
second_table_base_vector <- rep(0, 5 * number_of_groups)
dim(second_table_base_vector) <- c(number_of_groups, 5)

# Create an empty table.
second_table <- as.data.frame(second_table_base_vector)

# Fill the first and the second columns of the table with intervals information.
interval_length <- (max(x_vector) - min(x_vector)) / number_of_groups
second_table[1:number_of_groups, 1] <-
  min(x_vector) + (1:number_of_groups - 1) * interval_length
second_table[1:number_of_groups, 2] <-
  min(x_vector) + 1:number_of_groups * interval_length

# Fill the third column of the table with the number of values in the given 
# range.
for (i in 1:number_of_groups) {
  if (i != number_of_groups) {
    second_table[i, 3] <-
      length(which(x_vector >= second_table[i, 1] &
                     x_vector < second_table[i, 2]))
  } else {
    second_table[i, 3] <-
      length(which(x_vector >= second_table[i, 1] &
                     x_vector <= second_table[i, 2]))
  }
}

# Fill the fourth column of the table with sum of values in the given range.
for (i in 1:number_of_groups) {
  second_table[i, 4] <- 0
  for (j in 1:length(x_vector)) {
    if (x_vector[j] < second_table[i, 1]) next
    if (x_vector[j] > second_table[i, 2]) next
    if (x_vector[j] == second_table[i, 2] & i != number_of_groups) next
    second_table[i, 4] <- second_table[i, 4] + y_vector[j]
  }
}

# Fill the fifth column of the table with an average value in the given range.
second_table[1:number_of_groups, 5] <-
  second_table[1:number_of_groups, 4] / second_table[1:number_of_groups, 3]

# Set columns names.
colnames(second_table)<-c("Начало", "Конец", "Кол-во", "Сумма", "Среднее")

second_table

# ----------------------------------------------------------------------------
# Working the correlation characteristics and building the plot.

# Print the correlation coefficient (коэффициент корреляции).
cor(x_vector, y_vector)

# Print t value.
abs(cor(x_vector, y_vector)) * sqrt((length(x_vector) - 2) /
                                      1 - cor(x_vector, y_vector)^2)

# Build the dispersion pattern (диаграмма рассеивания).
plot(current_data, type = "p", main = "Корреляционное поле", 
     xlab = "X", ylab = "Y")

# Build the regression equation (уравнение регрессии).
lm(y_vector ~ x_vector)

# Add the regression line to the plot.
abline(lm(y_vector ~ x_vector))
