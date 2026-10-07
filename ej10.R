#Define the student's claimed slopes
slope_y_on_x <- 8 #(Ry/x = y = 5+8x)
slope_x_on_y <- -0.5 # Rx/y = y = 3 - 2x -> x = (3-y)/2

#Check the mathematical rule:
#The product of both slopes equals the squared correlation coefficient (r^2)
r_squared <- slope_y_on_x * slope_x_on_y
print(r_squared)

if(r_squared < 0){
  print("Conclusion : invalud, R squared cannot be negative")
} else if (r_squared > 1){
  print("Conclusion invalid, Correlation coefficient cannot be greater than 1")
} else{
  print("Conclusion: the answer is valid.")
}