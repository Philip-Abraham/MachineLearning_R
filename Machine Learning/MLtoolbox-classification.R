# Load the Sonar dataset
library(mlbench)
data(Sonar)

## A 60/40 split

# Shuffle row indices: rows
rows <- sample(nrow(Sonar))

# Randomly order data: Sonar
Sonar <- Sonar[rows, ]

# Identify row to split on: split
split <- round(nrow(Sonar) * .60)

# Create train
train <- Sonar[1:split, ]

# Create test
test <- Sonar[(split + 1):nrow(Sonar), ]


## Fit a logistic regression model

# Once you have your random training and test sets you can fit a logistic 
# regression model to your training set using the glm() function. glm() is a more 
# advanced version of lm() that allows for more varied types of regression models, 
# aside from plain vanilla ordinary least squares regression.
# Be sure to pass the argument family = "binomial" to glm() to specify that you 
# want to do logistic (rather than linear) regression.

# Fit glm model: model
model <- glm(Class ~ ., family = "binomial", train)
# Don't worry about warnings like glm.fit: algorithm did not converge or glm.fit: 
# fitted probabilities numerically 0 or 1 occurred. These are common on smaller 
# datasets and usually don't cause any issues. They typically mean your dataset is 
# perfectly seperable, which can cause problems for the math behind the model, but 
# R's glm() function is almost always robust enough to handle this case with no problems.

# Once you have a glm() model fit to your dataset, you can predict the outcome 
# (e.g. rock or mine) on the test set using the predict() function with the 
# argument type = "response"

# Predict on test: p
p <- predict(model, test, type = "response")


## Calculate a confusion matrix

# A confusion matrix is a very useful tool for calibrating the output of a model 
# and examining all possible outcomes of your predictions (true postive, true 
# negative, false positive, false negative).
# Before you make your confusion matrix, you need to "cut" your predicted 
# probabilities at a given threshold to turn probabilities into class predictions.

# # Turn probabilities into classes and look at their frequencies
# Calculate class probabilities: p_class
set.seed(1)
p_class <- ifelse(p > 0.50, "M", "R")
table(p_class)

# Make simple 2-way frequency table
table(p_class, test[["Class"]])

# Use caret's helper function to calculate additional statistics
# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


## Try another threshold

# In the previous exercises, you used a threshold of 0.50 to cut your predicted 
# probabilities to make class predictions (rock vs mine). However, this 
# classification threshold does not always align with the goals for a given 
# modeling problem.
# For example, pretend you want to identify the objects you are really certain are 
# mines. In this case, you might want to use a probability threshold of 0.90 to 
# get fewer predicted mines, but with greater confidence in each prediction.

# # Turn probabilities into classes and look at their frequencies
# Calculate class probabilities: p_class
set.seed(1)
p_class <- ifelse(p > 0.90, "M", "R")
table(p_class)

# Make simple 2-way frequency table
table(p_class, test[["Class"]])

# Use caret's helper function to calculate additional statistics
# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


## From probabilites to confusion matrix

# Conversely, say you want to be really certain that your model correctly 
# identifies all the mines as mines. In this case, you might use a prediction 
# threshold of 0.10, instead of 0.90.

# # Turn probabilities into classes and look at their frequencies
# Calculate class probabilities: p_class
set.seed(1)
p_class <- ifelse(p > 0.10, "M", "R")
table(p_class)

# Make simple 2-way frequency table
table(p_class, test[["Class"]])

# Use caret's helper function to calculate additional statistics
# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


## Plot an ROC curve

# A ROC curve is a really useful shortcut for summarizing the performance of a 
# classifier over all possible thresholds. This saves you a lot of tedious work 
# computing class predictions for many different thresholds and examining the 
# confusion matrix for each.
# The package for computing ROC curves is caTools, which contains a function 
# called colAUC(). This function is very user-friendly and can actually calculate 
# ROC curves for multiple predictors at once. In this case, you only need to 
# calculate the ROC curve for one predictor.

# Make an ROC curve using the predicted test set probabilities, p:
library(caTools)
colAUC(p, test[["Class"]], plotROC = TRUE)


## Customizing trainControl

# The area under the ROC curve is a very useful, single-number summary of a model's 
# ability to discriminate the positive from the negative class (e.g. mines from rocks). 
# An AUC of 0.5 is no better than random guessing, an AUC of 1.0 is a perfectly 
# predictive model, and an AUC of 0.0 is perfectly anti-predictive (which rarely happens).
# This is often a much more useful metric than simply ranking models by their 
# accuracy at a set threshold, as different models might require different 
# calibration steps (looking at a confusion matrix at each step) to find the 
# optimal classification threshold for that model.
# 
# You can use the trainControl() function in caret to use AUC (instead of acccuracy), 
# to tune the parameters of your models. The twoClassSummary() convenience function 
# allows you to do this easily.
# When using twoClassSummary(), be sure to always include the argument classProbs = TRUE 
# or your model will throw an error! (You cannot calculate AUC with just class 
# predictions. You need to have class probabilities as well.)

# Customize the trainControl object to use twoClassSummary rather than defaultSummary.
# Use 10-fold cross-validation.
# Be sure to tell trainControl() to return class probabilities.

# Create trainControl object: myControl
myControl <- trainControl(
        method = "cv",
        number = 10,
        summaryFunction = twoClassSummary,
        classProbs = TRUE, # IMPORTANT!
        verboseIter = TRUE
)


## Using custom trainControl

# Now that you have a custom trainControl object, it's easy to fit caret models 
# that use AUC rather than accuracy to tune and evaluate the model. You can just 
# pass your custom trainControl object to the train() function via the trControl argument.

# Train glm with custom trainControl: model
model <- train(
        Class ~ .,Sonar,
        method = "glm",
        trControl = myControl)



# Print model to console
model
# Don't worry about the warning messages generated by your code.
