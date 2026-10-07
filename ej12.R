x <- c(0, 1, 0, -1, 0, 1, 1, 2)
y <- c(0, 1, 1, 1, 3, 2, 2, 0)
z <- c(2, 3, 4, 5, 7, 4, 5, 0)

#Fit the plane using multiple linear regression
plane_model <- lm(z~x + y)

#Extract components needed 
N <- length(z)
residual_model <- residuals(plane_model)

SSE <- sum(residual_model^2)

residual_variance <- SSE / plane_model$df.residual

r_squared <- summary(plane_model)$r.squared

