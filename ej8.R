N <- 200

data <- matrix(c(
  30, 20, 
  60, 40, 
  40, 10
), nrow = 3, byrow = TRUE)

#Calculate the X^2 coefficient and the contingency coefficient C
#Step 1: Calculate the expected frequencies (E): 
# E = (Row Total * Column Total) / N

#Step 2: Claulate the x^2 (chi-square) coefficient
# x^2 = sum(((O - E)^2) / E)
#O is the observed value

#Step 3: Calculate the Contingency Coefficient (C)
# C = sqrt(X^2 / (X^2 + N))

chisq_result <- chisq.test(data, correct = FALSE)
chi2_value <- chisq_result$statistic
print(round(chi2_value, 4))

C_coefficient <- sqrt(chi2_value / (chi2_value + N))
print(round(C_coefficient, 4))