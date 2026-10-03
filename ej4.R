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