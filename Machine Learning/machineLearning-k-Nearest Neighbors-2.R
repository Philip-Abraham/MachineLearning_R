# You are given data on GDP per capita and its relation to the percentage of urban 
# population for several UN countries, measured in the year 2014 (Source: The World Bank). 

# Read in your data set and assign to the object:
world_bank_test<- read.table("./worldtest.txt", header  = TRUE)
world_bank_train<- read.table("./worldtrain.txt", header  = TRUE)

## Build a linear model, lm_wb

# Plot urb_pop as function of cgdp
plot(world_bank_train$cgdp, world_bank_train$urb_pop)

# Set up a linear model between the two variables: lm_wb
lm_wb <- lm(urb_pop~cgdp, data=world_bank_train)

# Add a red regression line to your scatter plot
abline(lm_wb$coefficients, col="red")

# Summarize lm_wb and select R-squared
summary(lm_wb)$r.squared


# In the last exercise, your scatter plot didn't show a strong linear relationship. 
# You confirmed this with the regression line and Rsquared

# To improve your model, take a step back and study the nature of the data. 
# The predictor variable is numerical, while the response variable is expressed in
# percentiles. It would make more sense if there were a linear relationship between 
# the percentile changes of the GDP / capita and the changes in the response.
# 
# To obtain an estimation of percentile changes, take the natural logarithm of the 
# GDP / capita and use this as your new predictor variable.
 
## Build the log-linear model

# Plot: change the formula and xlab
plot(urb_pop ~ log(cgdp), data = world_bank_train,
     xlab = "log(GDP per Capita)",
     ylab = "Percentage of urban population")

# Linear model: change the formula
lm_wb_log <- lm(urb_pop ~ log(cgdp), data = world_bank_train)

# Add a red regression line to your scatter plot
abline(lm_wb_log$coefficients, col = "red")

# Summarize lm_wb_log and select R-squared
summary(lm_wb_log)$r.squared


## Does the model generalize??

# Above, you were advised to take the logarithm of GDP per capita. Both the regression 
# line and R2R2 showed a better result for the log-linear model, lm_wb_log, than 
# for the simple linear model, lm_wb.

# You might be wondering whether we were misguiding you and had you produce an overfit? 
# Test if your model generalizes well. You'll be comparing the RMSE for train and test sets

# Calculate rmse_train
rmse_train <- sqrt(mean(lm_wb_log$residuals ^ 2))

# The real percentage of urban population in the test set, the ground truth
world_bank_test_truth <- world_bank_test$urb_pop

# The predictions of the percentage of urban population in the test set
world_bank_test_input <- data.frame(cgdp = world_bank_test$cgdp)
world_bank_test_output <- predict(lm_wb_log, world_bank_test_input)

# The residuals: the difference between the ground truth and the predictions
res_test <- world_bank_test_output - world_bank_test_truth


# Use res_test to calculate rmse_test
rmse_test <- sqrt(mean(res_test ^ 2))

# Print the ratio of the test RMSE over the training RMSE
rmse_test/rmse_train
# The test's RMSE is only slightly larger than the training RMSE. This means that 
# your model generalizes well to unseen observations. You can conclude the 
# logarithm transformation did improve your model. It fits your data better and 
# does a good job at generalizing!       

        
## Your own k-NN algorithm!
# This k-NN model is actually simply a function that takes test and training data 
# and predicts response variables on the fly: my_knn().

# We went ahead and defined a function my_knn that contains a k-NN algorithm. 
# Its arguments are:
# x_pred: predictor values of the new observations (this will be the cgdp column of world_bank_test),
# x: predictor values of the training set (the cgdp column of world_bank_train),
# y: corresponding response values of the training set (the urb_pop column of world_bank_train),
# k: the number of neighbors (this will be 30).
# The function returns the predicted values for your new observations (predict_knn).
# 
# You'll apply a k-NN algorithm to the GDP / capita of the countries in 
# world_bank_test to predict their percentage of urban population.

my_knn <- function(x_pred, x, y, k){
        m <- length(x_pred)
        predict_knn <- rep(0, m)
        for (i in 1:m) {
                
                # Calculate the absolute distance between x_pred[i] and x
                dist <- abs(x_pred[i] - x)
                
                # Apply order() to dist, sort_index will contain
                # the indices of elements in the dist vector, in
                # ascending order. This means sort_index[1:k] will
                # return the indices of the k-nearest neighbors.
                sort_index <- order(dist)
                
                # Apply mean() to the responses of the k-nearest neighbors
                predict_knn[i] <- mean(y[sort_index[1:k]])
                
        }
        return(predict_knn)
}
###

# world_bank_train and world_bank_test are pre-loaded

# Apply your algorithm on the test set: test_output
test_output <- my_knn(world_bank_test$cgdp, world_bank_train$cgdp, world_bank_train$urb_pop, 30)

# Have a look at the plot of the output
plot(world_bank_train,
     xlab = "GDP per Capita",
     ylab = "Percentage Urban Population")
points(world_bank_test$cgdp, test_output, col = "green")

# Now let's compare the k-NN results with the linear regression to see which one is the best model!


## Parametric vs non-parametric!

# So now you've build three different models for the same data: - a simple linear 
# model, lm_wb, - a log-linear model, lm_wb_log and - a non-parametric k-NN model. 
# You'll calculate the RMSE of the test set for the simple linear, log-linear and 
# k-NN regression. Have a look at the results, which regression approach performs the best?

# Define ranks to order the predictor variables in the test set
ranks <- order(world_bank_test$cgdp)

# Scatter plot of test set
plot(world_bank_test,
     xlab = "GDP per Capita", ylab = "Percentage Urban Population")

# Predict with simple linear model and add line
test_output_lm <- predict(lm_wb, data.frame(cgdp = world_bank_test$cgdp))
lines(world_bank_test$cgdp[ranks], test_output_lm[ranks], lwd = 2, col = "blue")

# Predict with log-linear model and add line
test_output_lm_log <- predict(lm_wb_log, data.frame(cgdp = world_bank_test$cgdp))
lines(world_bank_test$cgdp[ranks], test_output_lm_log[ranks], lwd = 2, col = "red")

# Predict with k-NN and add line
test_output_knn <- my_knn(world_bank_test$cgdp, world_bank_train$cgdp, world_bank_train$urb_pop, 30)
lines(world_bank_test$cgdp[ranks], test_output_knn[ranks], lwd = 2, col = "green")


# Calculate RMSE on the test set for simple linear model
sqrt(mean( (test_output_lm - world_bank_test$urb_pop) ^ 2))

# Calculate RMSE on the test set for log-linear model
sqrt(mean( (test_output_lm_log - world_bank_test$urb_pop) ^ 2))

# Calculate RMSE on the test set for k-NN technique
sqrt(mean( (test_output_knn - world_bank_test$urb_pop) ^ 2))

#### The log-linear model seems to give the best RMSE on the test set!
