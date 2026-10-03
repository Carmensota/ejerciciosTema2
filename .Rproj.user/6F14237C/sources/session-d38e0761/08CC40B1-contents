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