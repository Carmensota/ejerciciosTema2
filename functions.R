P <- c(1, 2, 3, 4, 5)
C <- c(1, 1, 2, 2, 4)

#Create the absolute joint frequency table
abs_table <- table(P, C)
print(abs_table)

#Add marginal totals 
abs_table_with_totals <- addmargins(abs_table)
print(abs_table_with_totals)

#Create the relative frequency table (divided by N = 5 total pieces)
rel_table <- prop.table(abs_table)
print(rel_table)





#When we are given table of absolute frequencies: 
counts <- matrix(c(
  2, 7, 12, 10, 4, 
  5, 14, 23, 15, 7, 
  12, 31, 23, 8, 3, 
  20, 18, 8, 2, 1
), nrow = 4, byrow = TRUE)

#Set the row (X) and column (Y) labels
dimnames(counts) <- list(x = c("0", "1", "2", "3"), 
                         y = c("-2", "-1", "0", "1", "2"))
freq_table <- as.table(counts)

#Print the original table with totals to verify
print(addmargins(freq_table))


#Distribution of y conditioned on x = 2
#(margin = 1 calculates row proportions; row total equals 1)
y_given_x2 <- prop.table(freq_table, margin = 1)["2", ]

print(y_given_x2)


#Distribution of x conditioned on y = -1
#(margin = 2 calculates column proportions; col total equals 1)
x_given_y_minus1 <- prop.table(freq_table, margin  =2)[, "-1"]

print(x_given_y_minus1)



#For calculating the mean of a variable conditioned by other, for example
#y conditioned by x = 5:

mean_yx5 <- mean(y[x == 5])





#c) Are the variables independent? Justify your answer
#Two variables are independent if and only if their joint frequencies equal the product
#of their marginal distributions. In base R, you can check this directly by 
#comparing the observed relative table with the expected independent table using outer()

#Get the global relative joint frequencies
rel_table <- prop.table(abs_table)

#Get the marginal probabilities for X and Y
margin_x <- margin.table(rel_table, 1)
margin_y <- margin.table(rel_table, 2)

#Create the expected distribution table IF they were independent
expected_independent <- outer(margin_x, margin_y, "*")

print(rel_table)
print(expected_independent)

#Check if they are completely identical 
is_independent <- all(abs(rel_table - expected_independent) <1e-9)
print(paste("Are variables independent?: ", is_independent))




data <- matrix(c(
  0.03, 0.07, 
  0.06, 0.14, 
  0.09, 0.21
), nrow = 3, byrow= TRUE)

dimnames(data) <- list(y = c("1", "3", "5"),
                       x = c("0", "1"))
freq_table <- as.table(data)
print(freq_table)

rel_table <- prop.table(data)
print(rel_table)


#Independency
marg_x <- margin.table(rel_table, 2)
marg_y <- margin.table(rel_table, 1)
theoretical_independency <- outer(marg_y, marg_x, "*")

is_independent <- all(abs(rel_table - theoretical_independency) < 1e-9)
print(paste("Are x and y independent: ", is_independent))


#Distributions (marginal of x and conditional x | y = 1)
dist_x_marginal <- marg_x
dist_x_conditioned <- prop.table(rel_table, margin = 1)["1", ]

print(dist_x_marginal)
print(dist_x_conditioned)


#Mean, median, mode
values_x <- as.numeric(names(dist_x_marginal))

mean <- sum(values_x * dist_x_marginal)
print(mean)

prob_accum <- cumsum(dist_x_marginal)
median <- values_x[which(prob_accum >= 0.5)[1]]
print(median)

mode_x <- values_x[which.max(dist_x_marginal)]
print(mode_x)










x <- c(2, 2, 3, 3, 4, 4, 5, 6, 7, 7)
y <- c(0, 1, 1, 1, 3, 3, 6, 7, 8, 7)

#a) Plot its scatter diagram and observe that, apparently, there is a good linear correlation
plot(x, y, 
     main = "Scatter diagram", 
     xlab = "hours worked", 
     ylab = "errors made", 
     pch = 19, col = "blue", las = 1)


#b) Calculate the coefficient of that linear correlation
r <- cor(x, y)
print(paste("Coefficient of correlation: ", round(r, 4)))


#c) Calculate the regression line of the errors on working hours
line_y_over_x <- lm(y ~ x)
print(line_y_over_x$coefficients)


#d) The regression line of working hours on errors
line_x_over_y <- lm(x ~ y)
print(line_x_over_y$coefficients)



#e) Draw both regression lines and check that they intersect at the point where the means
#of both variables coincide. 
abline(line_y_over_x, col = "blue", lwd = 2)   #Add line y over x


#Add line x over y. As x is dependent of y: x = a + b*y => y = (x-a)/b
a_x <- line_x_over_y$coefficients[1]
b_x <- line_x_over_y$coefficients[2] 
abline(a = -a_x/b_x, b = 1/b_x, col = "red", lwd = 2, lty = 2)

#Add point of means
mean_x <- mean(x)
mean_y <- mean(y)
points(mean_x, mean_y, col = "darkgreen", pch = 13, cex = 2, lwd = 3)



#f) How many errors can we expect from a worker who works 10 consecutive hours? 
pred_y <- predict(line_y_over_x, newdata = data.frame(x = 10))
print(paste("Errors predicted for 10 hours: ", round(pred_y, 2)))


#g) How many consecutive working hours can we expect a worker who makes 13 errors to have worked? 
pred_x <- predict(line_x_over_y, newdata = data.frame(y = 13))
print(paste("Hours predicted for 13 errors: ", round(pred_x, 2)))





#Calculate a logarithmic regression model 
#previously defined x and y (x <- c(...), y <- c(...))
data <- data.frame(
  X = x, 
  Y = y
)
log_model <- lm(y~log(x), data = data)
print(log_model)




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