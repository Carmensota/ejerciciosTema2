#Find the spearman correlation coefficient between the working hours and the number
#of errors commited in the table from the exercise 5
x <- c(2, 2, 3, 3, 4, 4, 5, 6, 7, 7)
y <- c(0, 1, 1, 1, 3, 3, 6, 7, 8, 7)

data <- data.frame(
  X = x, 
  Y = y
)

cor(data$X, data$Y, method = "spearman")
cor.test(data$X, data$Y, method = "spearman")

#Draw a scatter plot of the data
plot(data$X, data$Y, 
     xlab = "Working Hours", 
     ylab = "Errors", 
     pch = 19, 
     col = "blue")