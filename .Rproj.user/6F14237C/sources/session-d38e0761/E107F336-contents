N <- 20 
x <- c(5, 5, 5, 8, 5, 5, 8, 5, 5, 8, 5, 8, 5, 5, 8, 5, 5, 5, 8, 5)
y <- c(1, 0, 0, 1, 0, 1, 0, 1, 2, 1, 0, 0, 1, 0, 2, 1, 0, 2, 0, 0)

#a) Construct the joint frequency table
abs_table <- table(x, y)
print(abs_table)


#b) Determine the relative distribution of y conditioned on x = 5 and calculate its mean
y_given_x5 <- prop.table(abs_table, margin = 1)["5", ]

print(y_given_x5)

mean_yx5 <- mean(y[x == 5])
print(mean_yx5)



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