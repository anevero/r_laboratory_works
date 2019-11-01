# Read the data from the file.
current_data <- read.table(file="input_files/input_lab1.txt")
current_data

# Convert the data to the vector and then to the table.
data_vector <- current_data[,1]
data_table <- table(data_vector)
data_table

# Create data frame.
data_frame <- as.data.frame(data_table)

# Add third, fourth and fifth columns.
data_frame[,3] <- 100 * data_table / sum(data_frame[,2])
data_frame[,4] <- cumsum(data_frame[,2])
data_frame[,5] <- cumsum(data_frame[,3])

# Define columns names.
colnames(data_frame)<-c("Значение", "Частота", "Частость",
                        "Накопленная частота", "Накопленная частость")
data_frame

plot(data_table, type="l", main="Полигон частот",
     xlab="Значение", ylab="Частота")

# Sort the data vector and remove duplicates from it.
data_vector <- sort(data_vector)
data_vector <- unique(data_vector)

plot(data_vector, as.numeric(data_frame[,4]), type="l", xlab="Значение", 
     ylab="Накопленная частота", main= "Кумулята абсолютных частот")
plot(data_vector, as.numeric(data_frame[,5]), type="l", xlab="Значение", 
     ylab="Накопленная частость", main= "Кумулята относительных частот")

