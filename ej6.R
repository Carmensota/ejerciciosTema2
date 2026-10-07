x <-  c(1, 20, 50, 100, 200, 300, 500, 1000)
y <-  c(7, 8.5, 9, 9.4, 9.7, 9.9, 10.2, 10.5)

#a) Calculate its linear correlation coefficient and the regression line of Y on X
# coefficient of linear correlation: 
r <- cor(x, y)
print(round(r, 4))

#regression line of Y on X
line_y_over_x <- lm(y~x)
print(line_y_over_x$coefficients)

#b) Calculate a logarithmic regression model 
data <- data.frame(
  X = x, 
  Y = y
)
log_model <- lm(y~log(x), data = data)
print(log_model)


#c) Find an expected value for X = 400
pred_y <- predict(line_y_over_x, newdata = data.frame(x = 400))
print(pred_y)