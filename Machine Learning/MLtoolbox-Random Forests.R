## Fit a random forest-1

# Random forest models are much more flexible than linear models, and can model 
# complicated nonlinear effects as well as automatically capture interactions 
# between variables. They tend to give very good results on real world data. 

# Load some data
library(caret)
library(mlbench)
data(Sonar)

# Set seed
set.seed(42)

# Fitting a random forest model is exactly the same as fitting a generalized 
# linear regression model. You simply change 
# the method argument in the train function to be "ranger". The ranger package is 
# a rewrite of R's classic randomForest package and fits models much faster, but 
# gives almost exactly the same results. We suggest that all beginners use the 
# ranger package for random forest modeling.

# Fit a model
model <- train(Class~.,
                 data = Sonar,
                 method = "ranger"
)

# Print model to console
model

# Plot the results
plot(model)



## Fit a random forest-2

# Train a random forest called model on the Sonar dataset, such that Class is the 
# response variable and all other variables are explanatory variables.
# Use method = "ranger".
# Use a tuneLength of 1.
# Use 5 CV folds.

# Fit random forest: model
model_2 <- train(
        Class~.,
        tuneLength = 1,
        data = Sonar, method ="ranger",
        trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model_2


## Random forests require tuning

# Random forest models have a primary tuning parameter of mtry, which controls 
# how many variables are exposed to the splitting search routine at each split. 
# For example, suppose that a tree has a total of 10 splits and mtry = 2. This 
# means that there are 10 samples of 2 predictors each time a split is evaluated.

# Hyperparameters control how the model is fit
# Most important is mtry
# Number of randomly selected variables used at each split. Between 2 and 100 variables
# Lower value(2 variables) = more random
# Higher value (100 variables) = less random
# Hard to know the best value in advance

# Select hyperparameters based on out-of-sample error using the caret package
# Fit a model with a deeper tuning grid - tuneLength = 10
model_10 <- train(Class~., data = Sonar,
                 method = "ranger", tuneLength = 10) # default tunelength = 3
# Plot the results (the x-axis shows the mtry values)
plot(model_10)

# Fit random forest: model: Try a shorter tune length - tuneLength = 3. Use 5 CV folds.
model_3 <- train(
        Class~.,
        tuneLength = 3,
        data = Sonar, method ="ranger",
        trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model_3

# Plot model
plot(model_3)


# Fit a random forest with custom tuning

# Now that you've explored the default tuning grids provided by the train() 
# function, let's customize your models a bit more.
# You can provide any number of values for mtry, from 2 up to the number of 
# columns in the dataset. In practice, there are diminishing returns for much 
# larger values of mtry.

# So you will use a custom tuning grid that explores mtry = 2, 3, 4, 5, 10, 20.
# Higher the mtry value, the higher the complexity.

# Define a custom tuning grid
myGrid <- data.frame(mtry = c(2, 3, 4, 5, 10, 20))

# Fit a model with a custom tuning grid with 5 CV folds
set.seed(42)
model_c <- train(Class ~ ., data = Sonar, method = "ranger",
                 tuneGrid = myGrid,
                 trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model_c

# Plot the results
plot(model_c)
