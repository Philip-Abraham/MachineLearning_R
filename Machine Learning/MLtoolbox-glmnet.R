## Introducing glmnet

# Extension of glm models with built-in variable selection
# 
# Helps deal with collinearity and small samples sizes
# 
# Two primary forms:
# Lasso regression (Penalizes number of non-zero coefficients), and 
# Ridge regression (Penalizes absolute magnitude of coefficients)
# 
# Attempts to find a parsimonious (i.e. simple) model
# 
# Pairs well with random forest models


## Make a custom trainControl

# You are looking at a classification problem. This is a simulated dataset based 
# on the "don't overfit" competition on Kaggle a number of years ago.
# Classification problems are a little more complicated than regression problems 
# because you have to provide a custom summaryFunction to the train() function to 
# use the AUC metric to rank your models.

# Load data
overfit <- read.csv("./overfit_rev.csv", header = TRUE)

for(i in 1:nrow(overfit)){
        if(overfit$y[i]==0){
                overfit$y[i]<-c("class1")
        }else{overfit$y[i]<-c("class2")}
                
}
overfit$y <- factor(overfit$y)

library(caret)
# Make a custom trainControl
myControl <- trainControl(
        method = "cv", number = 10,
        summaryFunction = twoClassSummary,
        classProbs = TRUE, # Super important!
        verboseIter = TRUE
)

# Fit glmnet with custom trainControl

# Now that you have a custom trainControl object, fit a glmnet model to the 
# "don't overfit" dataset. The glmnet is an extention of the generalized linear 
# regression model (or glm) that places constraints on the magnitude of the 
# coefficients to prevent overfitting. This is more commonly known as "penalized" 
# regression modeling and is a very useful technique on datasets with many 
# predictors and few values.

# Fit glmnet model: model - Try the defaults(3 values of alpha & 3 values of lambda)
set.seed(42)
model_g <- train(
        y ~ ., overfit,
        method = "glmnet",
        trControl = myControl
)

# Print model to console
model_g

# Print maximum ROC statistic
max(model_g[["results"]])

# Plot results
plot(model_g)


# Custom tuning glmnet models
# 2 tuning parameters: alpha and lambda
# For single alpha, all values of lambda fit simultaneously
# Many models for the "price" of one

# The glmnet model actually fits many models at once (one of the great things 
# about the package). You can exploit this by passing a large number of lambda 
# values, which control the amount of penalization in the model. train() is smart 
# enough to only fit one model per alpha value and pass all of the lambda values 
# at once for simultaneous fitting.

# Make a custom tuning grid - fit 10 models per value of alpha.
# You also look at the two forms of penalized models with this tuneGrid: ridge 
# regression and lasso regression. alpha = 0 is pure ridge regression, and 
# alpha = 1 is pure lasso regression. 
myGrid <- expand.grid(
        alpha = 0:1,
        lambda = seq(0.0001, 0.1, length = 10)
)
# Fit a model
set.seed(42)
model_gc <- train(y ~ ., overfit, method = "glmnet",
                 tuneGrid = myGrid, trControl = myControl)
# Plot results
plot(model_gc)

# Print model to console
model_gc

# Print maximum ROC statistic
max(model_gc[["results"]][["ROC"]])

# Full regularization path for all the models with alpha=0
plot(model_gc$finalModel, label = TRUE)
# left side of plot (intercept only model-high penalty) has high lambda values, 
# and the right side of plot (full model with no penalty ) has low lambda value.
# Regression coefficients are shrunk from right to left as you increase the 
# strength on the penalty, therefore decrease the complexity of the model.
