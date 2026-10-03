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