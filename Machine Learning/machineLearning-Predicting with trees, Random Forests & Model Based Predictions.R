# Load the cell segmentation data from the AppliedPredictiveModeling package
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

# Subset the data to a training set and testing set based on the Case variable in the data set.
training <- subset(segmentationOriginal, segmentationOriginal$Case=="Train")
testing <- subset(segmentationOriginal, segmentationOriginal$Case=="Test")
dim(training); dim(testing)

# fit a CART model with the rpart method using all predictor variables and 
# default caret settings.
set.seed(125)
modFit <- train(Class ~ .,method="rpart",data=training)
print(modFit$finalModel)

# plot model
library(rattle)
fancyRpartPlot(modFit$finalModel)


## Load the olive oil data
library(pgmm)
data(olive)
olive = olive[,-1]

# These data contain information on 572 different Italian olive oils from multiple 
# regions in Italy. Fit a classification tree where Area is the outcome variable. 
library(tree)
tr <- tree(Area ~ ., olive)
tr

# Plot tree
plot(tr,type="u");text(tr,pretty=0)

# predict the value of area for the following data frame:
newdata = as.data.frame(t(colMeans(olive)))
pred_g <- predict(tr, newdata) # this is not right...Area should be qualitative, not numeric


## Load the South Africa Heart Disease Data and create training and test sets
library(ElemStatLearn)
data(SAheart)
set.seed(8484)

train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# fit a logistic regression model
set.seed(13234)
library(caret)
model <- glm(chd ~ age+alcohol+obesity+tobacco+typea+ldl, family = "binomial", trainSA)

# Predict on test: pts
pts <- predict(model, testSA, type = "response")

# # Turn probabilities into classes and look at their frequencies
# Calculate class probabilities: p_classts
set.seed(13234)
p_classts <- ifelse(pts > 0.50, "1", "0")
table(p_classts)

# Make simple 2-way frequency table
table(p_classts, testSA[["chd"]])

# Use caret's helper function to calculate additional statistics
# Create confusion matrix
CMtest <- confusionMatrix(p_classts, testSA[["chd"]])
CMtest

# What is the accuracy on the test set?
CMtest$overall[1]
# What is the misclassification rate on the test set?
as.numeric(1-CMtest$overall[1])


# Predict on train: ptr
ptr <- predict(model, trainSA, type = "response")

## What is the misclassification rate on the train set?
# # Turn probabilities into classes and look at their frequencies
# Calculate class probabilities: p_classtr
set.seed(13234)
p_classtr <- ifelse(ptr > 0.50, "1", "0")
table(p_classtr)

# Make simple 2-way frequency table
table(p_classtr, trainSA[["chd"]])

# Use caret's helper function to calculate additional statistics
# Create confusion matrix
CMtrain <- confusionMatrix(p_classtr, trainSA[["chd"]])
CMtrain

# What is the accuracy on the train set?
CMtrain$overall[1]
# What is the misclassification rate on the train set?
as.numeric(1-CMtrain$overall[1])


## Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

dfte <- vowel.test
dftr <- vowel.train

dfte$y <- factor(dfte$y)
dftr$y <- factor(dftr$y)

# Fit a random forest predictor relating the factor variable y to the remaining variables
library (randomForest)
set.seed (33833)
bag.vowelTR =randomForest(y ~.,data=dftr)
bag.vowelTR
imp <- importance(bag.vowelTR)
library(caret)
result <- varImp(bag.vowelTR)
order(result)
varImpPlot (bag.vowelTR)

