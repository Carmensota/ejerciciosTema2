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