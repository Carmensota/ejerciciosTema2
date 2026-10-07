#Determine the covariance and Rx/y, Ry/x

midpoint_x <- c((150+160)/2, (160+170)/2, (170+180)/2, (180+190)/2, (190+200)/2)
midpoint_y <- c((50+60)/2, (60+70)/2, (70+80)/2, (80+90)/2, (90+100)/2, (100+110)/2)

frequencies <- matrix(c(
  2, 0, 0, 0, 0, 0,
  4, 4, 0, 0, 0, 0, 
  1, 6, 3, 1, 0, 0, 
  0, 1, 4, 1, 1, 0, 
  0, 0, 0, 0, 1, 1
), nrow = 5,ncol = 6,  byrow = TRUE)

#Expand the matrix into the raw data vectors X and Y based on their frequencies
X_raw <- numeric()
Y_raw <- numeric()

for(i in 1:length(midpoint_x)){
  for(j in 1:length(midpoint_y)){
    count <- frequencies[i, j]
    if(count > 0){
      X_raw <- c(X_raw, rep(midpoint_x[i], count))
      Y_raw <- c(Y_raw, rep(midpoint_y[j], count))
    }
  }
}


#Calculations:
#Covariance (s_xy) using the population denominator
N <- sum(frequencies)
covariance <- cov(X_raw, Y_raw) * (N-1) / N
print(round(covariance, 2))

#Regression line of Y on X
line_y_over_x <- lm(Y_raw~X_raw)
print(line_y_over_x)

#Regression line of X on Y
line_x_over_y <- lm(X_raw~Y_raw)
print(line_x_over_y)