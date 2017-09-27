## Dealing with missing values

# median imputation - Replace missing values with medians
# Works well if data missing at random

# The train() function in caret contains an argument called preProcess, which allows 
# you to specify that median imputation should be used to fill in the missing 
# values. Specify the x and y arguments to train(), where x is an object with 
# samples in rows and features in columns and y is a numeric or factor vector 
# containing the outcomes. Said differently, x is a matrix or data frame that 
# contains the whole dataset you'd use for the data argument to the lm() call, 
# for example, but excludes the response variable column; y is a vector that 
# contains just the response variable column.

# Generate some data with missing values
data(mtcars)
set.seed(42)
mtcars[sample(1:nrow(mtcars), 10), "hp"] <- NA
# Split target from predictors
Y <- mtcars$mpg
X <- mtcars[, 2:4]


## median imputation - Replace missing values with medians
# Works well if data missing at random
# Now fit with median imputation
library(caret)
model_m <- train(x = X, y = Y, preProcess = "medianImpute")
print(model_m)


## Use KNN imputation
# Median imputation is fast, but Can produce incorrect results if data missing not at random
# k-nearest neighbors (KNN) imputation
# Imputes based on "similar" non-missing rows

# Example: missing not at random
# Generate data with missing values
data(mtcars)
mtcars[mtcars$disp < 140, "hp"] <- NA
Y <- mtcars$mpg
X <- mtcars[, 2:4]

# Pretend smaller cars don't report horsepower
# Median imputation incorrect in this case - Assumes small cars have medium-large horsepower
# Using median imputation
set.seed(42)
model_m1 <- train(x = X, y = Y, method = "glm",
                 preProcess = "medianImpute")
print(min(model_m1$results$RMSE))

# KNN imputation is better
# Uses cars with similar disp / cyl to impute
# Yields a more accurate (but slower) model

# Use KNN imputation
set.seed(42)

library(RANN)
model_k <- train(x = X, y = Y,
                 method = "glm",
                 preProcess = "knnImpute"
)
print(min(model_k$results$RMSE))


## Compare KNN and median imputation
# All of the preprocessing steps in the train() function happen in the training 
# set of each cross-validation fold, so the error metrics reported include the 
# effects of the preprocessing.This includes the imputation method 
# used (e.g. knnImputate or medianImpute). This is useful because it allows you to 
# compare different methods of imputation and choose the one that performs the 
# best out-of-sample.
dotplot(resamples(x=list(median_model=model_m1, knn_model=model_k)), metric = "RMSE")


## The wide world of preProcess
# You can do a lot more than median or knn imputation
# Can chain together multiple preprocessing steps
# Common "recipe" for linear models (order matters) - Median/KNN imputation -> center -> scale -> PCA/spatial sign -> fit glm

# Generate some data with missing values
data(mtcars)
set.seed(42)
mtcars[sample(1:nrow(mtcars), 10), "hp"] <- NA
Y <- mtcars$mpg
X <- mtcars[,2:4]
# Use linear model "recipe"
set.seed(42)
model_1 <- train(
        x = X, y = Y, method = "glm",
        preProcess = c("medianImpute", "center", "scale")
)
print(min(model_1$results$RMSE))

# PCA before modeling
set.seed(42)
model_p <- train(
        x = X, y = Y, method = "glm",
        preProcess = c("medianImpute", "center", "scale", "pca")
)
min(model_p$results$RMSE)

# Spatial sign transform
set.seed(42)
model_s <- train(
        x = X, y = Y, method = "glm",
        preProcess = c("medianImpute", "center", "scale", "spatialSign"))
min(model_s$results$RMSE)


## No (or low) variance variables
# Some variables don't contain much information
# Constant (i.e. no variance)
# Nearly constant (i.e. low variance)
# Easy for one fold of CV to end up with constant column
# Can cause problems for your models
# Usually remove extremely low variance variables

# Example: constant column in mtcars
# Load dataset
data(mtcars)
set.seed(42)
mtcars[sample(1:nrow(mtcars), 10), "hp"] <- NA
Y <- mtcars$mpg
X <- mtcars[, 2:4]
# Add constant-valued column to mtcars
X$bad <- 1

# "zv" removes constant columns
# "nzv" removes nearly constant columns
# Have caret remove those columns during modeling
set.seed(42)
model_bd <- train(
        x = X, y = Y, method = "glm",
        preProcess = c("zv", "medianImpute", "center", "scale", "pca")
)
min(model_bd$results$RMSE)


## Principle components analysis
# Combines low-variance and correlated variables
# Single set of high-variance, perpendicular predictors
# Prevents collinearity (i.e. correlation among predictors)
## Using PCA as an alternative to nearZeroVar()
# An alternative to removing low-variance predictors is to run PCA on your dataset. 
# This is sometimes preferable because it does not throw out all of your data: 
# many different low variance predictors may end up combined into one high variance 
# PCA variable, which might have a positive impact on your model's accuracy.
# This is an especially good trick for linear models: the pca option in the preProcess 
# argument will center and scale your data, combine low variance variables, and 
# ensure that all of your predictors are orthogonal. This creates an ideal dataset 
# for linear regression modeling, and can often improve the accuracy of your models.

# Example: blood-brain data - Lots of predictors. Many of them low-variance
# Load the blood brain dataset
data(BloodBrain)
names(bbbDescr)[nearZeroVar(bbbDescr)]

## Comparing three methods

# Basic model
set.seed(42)
model <- train(
        x = bbbDescr, y = logBBB, method = "glm",
        trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
        preProcess = c("zv", "center", "scale")
)
min(model$results$RMSE)

# Remove low-variance predictors
set.seed(42)
model <- train(
        x = bbbDescr, y = logBBB, method = "glm",
        trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
        preProcess = c("nzv", "center", "scale")
)
min(model$results$RMSE)

# Add PCA
set.seed(42)
model <- train(
        x = bbbDescr, y = logBBB, method = "glm",
        trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
        preProcess = c("zv", "center", "scale", "pca")
)
min(model$results$RMSE)
# Note that the PCA model's accuracy is slightly higher than the nearZeroVar() 
# model calculated previously above. PCA is generally a better method for handling 
# low-information predictors than throwing them out entirely.