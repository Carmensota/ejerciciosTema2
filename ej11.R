line_x_over_y <- 2
line_y_over_x <- 2/9

N <- 10
variance_x <- 9

#a) Find the linear correlation coefficient, the variance of Y, and the covariance
r <- sqrt(line_y_over_x * line_x_over_y)
covariance <- line_y_over_x * variance_x
variance_y <- covariance / line_x_over_y

print(round(r, 4))
print(round(covariance, 4))
print(round(variance_y , 4))

#b) If it is discovered that one of the considered points, (2, -1), should not have been used, 
#find the new regression lines

#Find the initial means (intersection of x - 2y = 4 and 2x - 9y = 8)
#Solving the system of equations gives: 
mean_x <- 4
mean_y <- 0

#Calcuate initial sum totals using algebraic definitions
sum_x <- N * mean_x
sum_y <- N * mean_y
sum_x2 <- N * (variance_x + mean_x^2)
sum_y2 <- N * (variance_y + mean_y^2)
sum_xy <- N * (covariance + mean_x * mean_y)

#Remove the bad point (2, -1)
bad_x <- 2
bad_y <- -1

N_new <- N - 1
sum_x_new <- sum_x - bad_x
sum_y_new <- sum_y - bad_y
sum_x2_new <- sum_x2 - bad_x^2
sum_y2_new <- sum_y2 - bad_y^2
sum_xy_new <- sum_xy - (bad_x * bad_y)

#Calculate new means, variances, and covariance
mean_x_new <- sum_x_new / N_new
mean_y_new <- sum_y_new / N_new

var_x_new <- (sum_x2_new / N_new) - mean_x_new^2
var_y_new <- (sum_y2_new / N_new) - mean_y_new^2
cov_xy_new <- (sum_xy_new /N_new) - (mean_x_new * mean_y_new)

#Compute new slopes and intercepts
m1 <- cov_xy_new / var_x_new
b1 <- mean_y_new - m1*mean_x_new

m2 <- cov_xy_new / var_y_new
b2 <- mean_x_new - m2*mean_y_new

print(round(m1, 4))
print(round(b1, 4))
print(round(m2, 4))
print(round(b2, 4))
