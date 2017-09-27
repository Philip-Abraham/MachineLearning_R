## In-sample RMSE for linear regression on diamonds

# The diamonds dataset contains physical attributes of diamonds as well as the 
# price they sold for. One interesting modeling challenge is predicting diamond 
# price based on their attributes using something like a linear regression.
library(ggplot2)
data("diamonds")

# Fit lm model: model
model <- lm(price ~ ., diamonds)

# Predict on full data: p
p <- predict(model, diamonds)

# Compute errors using the formula errors=predicted???actual: error
error <- p - diamonds$price

# Calculate RMSE
sqrt(mean((p - diamonds$price)^2))


## Randomly order the data frame

# One way you can take a train/test split of a dataset is to order the dataset 
# randomly, then divide it into the two sets. This ensures that the training set 
# and test set are both random samples and that any biases in the ordering of the 
# dataset (e.g. if it had originally been ordered by price or size) are not 
# retained in the samples we take for training and testing your models. 
# You can think of this like shuffling a brand new deck of playing cards 
# before dealing hands.

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
diamonds <- diamonds[rows, ]


## Try an 80/20 split

# Now that your dataset is randomly ordered, you can split the first 80% of it 
# into a training set, and the last 20% into a test set. You can do this by 
# choosing a split point approximately 80% of the way through your data.

# Determine row to split on: split
split <- round(nrow(diamonds) * .80)

# Create train
train <- diamonds[1:split, ]

# Create test
test <- diamonds[(split + 1):nrow(diamonds), ]


## Predict on test set

# Fit lm model on train: model
model <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model, test)


## Calculate test set RMSE by hand

# Now that you have predictions on the test set, you can use these predictions to 
# calculate an error metric (in this case RMSE) on the test set and see how the 
# model performs out-of-sample, rather than in-sample as you did in the first exercise. 

# Compute errors: error
error <- test$price - p

# Calculate RMSE
sqrt(mean(error^2))


## 10-fold cross-validation

# A better approach to validating models is to use multiple systematic test sets, 
# rather than a single random train/test split. Fortunately, the caret package 
# makes this very easy to do.
# The caret package supports many types of cross-validation, and you can specify 
# which type of cross-validation and the number of cross-validation folds with 
# the trainControl() function, which you pass to the trControl argument in train().

library(caret)

# Fit lm model using 10-fold CV: model
model <- train(
        price~.,diamonds,
        method = "lm",
        trControl = trainControl(
                method = "cv", number = 10,
                verboseIter = TRUE
        )
)

# Print model to console
model


# You can do more than just one iteration of cross-validation. Repeated 
# cross-validation gives you a better estimate of the test-set error. You can also 
# repeat the entire cross-validation procedure. This takes longer, but gives you 
# many more out-of-sample datasets to look at and much more precise assessments of 
# how well the model performs.

# Re-fit the linear regression model to the diamonds dataset
# Use 5 repeats of 10-fold cross-validation.
model <- train(
        price~.,diamonds,
        method = "lm",
        trControl = trainControl(
                method = "cv", number = 10,
                repeats = 5,
                verboseIter = TRUE)
        )
        
# Print model to console
model


## Making predictions on new data

# Finally, the model you fit with the train() function has the exact same 
# predict() interface as the linear regression models you fit earlier above.
# After fitting a model with train(), you can simply call predict() with new data.

# Use the predict() function to make predictions with model on the full diamonds dataset.
pred <- predict(model, diamonds)
