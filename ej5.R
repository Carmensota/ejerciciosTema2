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

