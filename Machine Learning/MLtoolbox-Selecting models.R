## Make custom train/test indices

# The churn dataset contains data on a variety of telecom customers and the 
# modeling challenge is to predict which customers will cancel their service (or churn).

# Summarize the target variables
library(caret)
library(C50)
data(churn)
table(churnTrain$churn) / nrow(churnTrain)

# In this exercise, you will be exploring two different types of predictive models: 
# glmnet and rf, so the first order of business is to create a reusable 
# trainControl object you can use to reliably compare them.

# The data: customer churn at telecom company
# Fit different models and choose the best
# Models must use the same training/test splits
# Create a shared trainControl object

# Create train/test indexes
set.seed(42)
myFolds <- createFolds(churnTrain$churn, k = 5)

# Compare class distribution
i <- myFolds$Fold1
table(churnTrain$churn[i]) / length(i)

# Use folds to create a trainControl object - Exact same cross-validation folds for each model
myControl <- trainControl(
        summaryFunction = twoClassSummary,
        classProbs = TRUE,
        verboseIter = TRUE,
        savePredictions = TRUE,
        index = myFolds
)


## glmnet on churn data
# Now that you have a reusable trainControl object called myControl, you can start 
# fitting different predictive models to your churn dataset and evaluate their 
# predictive accuracy.
# glmnet, which penalizes linear and logistic regression models on the size and 
# number of coefficients to help prevent overfitting.  glmnet advantages are:
# Linear model with built-in variable selection
# Great baseline model
# Advantages
# Fits quickly
# Ignores noisy variables
# Provides interpretable coefficients

# Fit the model
set.seed(42)
model_glmnet <- train(
        churn ~ ., churnTrain,
        metric = "ROC",
        method = "glmnet",
        tuneGrid = expand.grid(
                alpha = 0:1,
                lambda = 0:10/10
        ),
        trControl = myControl
)

# Print model to console
model_glmnet

# Plot the results
plot(model_glmnet)

# Plot the coefficients
plot(model_glmnet$finalModel)
# left side of plot (intercept only model-high penalty) has high lambda values, 
# and the right side of plot (full model with no penalty ) has low lambda value.
# Regression coefficients are shrunk from right to left as you increase the 
# strength on the penalty, therefore decrease the complexity of the model.


## Random Forest on churn data
# Slower to fit than glmnet
# Less interpretable
# Often (but not always) more accurate than glmnet
# Easier to tune
# Require li$le preprocessing
# Capture threshold effects and variable interactions

set.seed(42)
churnTrain$churn <- factor(churnTrain$churn, levels = c("no", "yes"))
model_rf <- train(
        churn ~ ., churnTrain,
        metric = "ROC",
        method = "ranger",
        trControl = myControl
)
plot(model_rf)


## Comparing models - resamples() on churn data
# Make sure they were fit on the same data
# Selection criteria:
# Highest average AUC - In general, you want the model with the higher median AUC, as well as a smaller range between min and max AUC.
# Lowest standard deviation in AUC
# The resamples() function is your friend

# Make a list
model_list <- list(
        glmnet = model_glmnet,
        rf = model_rf
)
# Collect resamples from the CV folds
resamps <- resamples(model_list)
resamps

# Summarize the results
summary(resamps)


## Comparing models - caretEnsemble package

# Box-and-whisker
bwplot(resamps, metric = "ROC")

# Dot plot
dotplot(resamps, metric = "ROC")

# Density plot
densityplot(resamps, metric = "ROC")

# Scatter plot
# This plot shows you how similar the two models' performances are on different folds.
# It's particularly useful for identifying if one model is consistently better 
# than the other across all folds, or if there are situations when the inferior 
# model produces better predictions on a particular subset of the data.
xyplot(resamps, metric = "ROC")


# caretEnsemble provides the caretList() function for creating multiple caret 
# models at once on the same dataset, using the same resampling folds. 
library(caretEnsemble)

# caretList contains the glmnet and ranger models you fit on the churn dataset. 
# Use the caretStack() function to make a stack of caret models, with the two 
# sub-models (glmnet and ranger) feeding into another (hopefully more accurate!) 
# caret model.
model_list <- caretList(
        churn ~ ., data=churnTrain,
        trControl=myControl,
        methodList=c("glm", "ranger")
)

# Create ensemble model: stack
stack <- caretStack(model_list, method = "glm")

# Look at summary
summary(stack)
