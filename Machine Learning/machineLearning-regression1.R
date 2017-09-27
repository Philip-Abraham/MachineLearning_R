## Describe the linear relationship between the grey kangaroo's nose width (mm) and nose length (mm)

kang_nose <- read.csv("./kang.csv", header  = TRUE)

# Plot nose length as function of nose width.
plot(kang_nose, xlab = "nose width", ylab = "nose length")

# Fill in the ___, describe the linear relationship between the two variables: lm_kang
lm_kang <- lm(nose_length ~ nose_width, data = kang_nose)

# Print the coefficients of lm_kang
lm_kang$coefficients

# Predict and print the nose length of the escaped kangoroo
nose_width <- c(250)
nose_width_new <- data.frame(nose_width)
predict(lm_kang, nose_width_new)
# There is a visible linear pattern between the nose width and length of Grey 
# Kangaroos. With a nose width of 250mm, your prediction tells us Skippy has a 
# nose length of around 700mm. 


## Performance measure: RMSE

# Build model and make plot
lm_kang <- lm(nose_length ~ nose_width, data=kang_nose)
plot(kang_nose, xlab = "nose width", ylab = "nose length")
abline(lm_kang$coefficients, col = "red")

# Apply predict() to lm_kang: nose_length_est
nose_length_est <- predict(lm_kang)

# Calculate difference between the predicted and the true values: res
res <- kang_nose$nose_length - nose_length_est

# Calculate RMSE, assign it to rmse and print it
rmse <- sqrt(mean(res^2))
rmse
# The regression line passes well through the datapoints! What do you think of the 
# RMSE value: 43mm? High or low? It's difficult to interpret it if you have no 
# other model to compare with. In the next exercise you'll address this by 
# calculating the R-squared.


## Performance measures: R-squared

# Calculate the residual sum of squares: ss_res
ss_res <- sum(res^2)

# Determine the total sum of squares: ss_tot
ss_tot <- sum((kang_nose$nose_length-mean(kang_nose$nose_length))^2)

# Calculate R-squared and assign it to r_sq. Also print it.
r_sq <- 1-(ss_res/ss_tot)
r_sq

# Apply summary() to lm_kang
summary(lm_kang)
# Apart from rounding, the Multiple R-squared is exactly the same as the r_sq you 
# calculated. An R-squared of 0.77 is pretty neat!