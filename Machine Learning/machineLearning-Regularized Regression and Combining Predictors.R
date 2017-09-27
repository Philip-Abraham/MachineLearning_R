## Load the vowel.train and vowel.test data sets:
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

dfte <- vowel.test
dftr <- vowel.train

dfte$y <- factor(dfte$y)
dftr$y <- factor(dftr$y)

# Fit a random forest predictor relating the factor variable y to the remaining variables
library (caret)
set.seed (33833)
modrf <- train(y~ .,data=dftr, method="rf")
modrf
plot(modrf)

# Fit a boosted predictor using the "gbm" method
set.seed (33833)
modgb <- train(y~ .,data=dftr, method="gbm", verbose=FALSE)
modgb
plot(modgb)

# Comparing models
# Make a list of models
model_list <- list(
        rf = modrf,
        gb = modgb
)

# Collect resamples from the CV folds
resamps <- resamples(model_list)
resamps

# Summarize the results
summary(resamps)

# Box-and-whisker
bwplot(resamps)

# Dot plot
dotplot(resamps)

# Density plot
densityplot(resamps)

# Scatter plot
# This plot shows you how similar the two models' performances are on different folds.
# It's particularly useful for identifying if one model is consistently better 
# than the other across all folds, or if there are situations when the inferior 
# model produces better predictions on a particular subset of the data.
xyplot(resamps)

# What are the accuracies for the two approaches on the test data set? 
predrf <- predict(modrf, dfte)
table(predrf, dfte$y)
confusionMatrix(predrf, dfte[["y"]])
library(pROC)
auc_rf <- multiclass.roc(as.numeric(dfte$y), as.numeric(predrf))
print(auc_rf$auc) # Multi-class area under the curve: 0.8167


predgb <- predict(modgb, dfte)
table(predrf, dfte$y)
confusionMatrix(predgb, dfte[["y"]])
library(pROC)
auc_gb <- multiclass.roc(as.numeric(dfte$y), as.numeric(predgb))
print(auc_gb$auc) # Multi-class area under the curve: 0.7801

# Plot the two classifier predictons
library(ggplot2)
qplot(predrf,predgb,colour=y,data=dfte)

# What is the accuracy among the test set samples where the two methods agree?
tab <- table(predrf, predgb)
tab
df <- data.frame(as.integer(dfte$y), as.integer(predrf), as.integer(predgb))
names(df) <- c("y", "rf", "gb")
plot(df)

dfagree <- subset(df, df$rf==df$gb)
dfagreey <- subset(dfagree, dfagree$rf==dfagree$y) # rf(or gb) agreeing with test samples
plot(dfagree)
plot(dfagreey)
# agreement accuracy
nrow(dfagreey)/nrow(dfagree)

################################################################################
## Load the Alzheimer's data
library(caret)
library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]
testing = adData[-inTrain,]

# Predict diagnosis with all the other variables using a random forest ("rf"), 
# boosted trees ("gbm") and linear discriminant analysis ("lda") model.

# Build three different models
library(ggplot2)

set.seed(62433)
rfmod <- train(diagnosis ~.,method="rf",
              data=training)
summary(rfmod)
rfmod
plot(rfmod)

set.seed(62433)
gbmmod <- train(diagnosis ~ ., method="gbm",data=training,verbose=FALSE)
summary(gbmmod)
gbmmod
plot(gbmmod)

set.seed(62433)
ldamod <- train(diagnosis ~ ., method="lda",data=training,verbose=FALSE)
summary(ldamod)
ldamod

# Comparing models
# Make a list of models
model_list <- list(
  rf = rfmod,
  gbm = gbmmod,
  lda = ldamod
)

# Collect resamples from the CV folds
resamps <- resamples(model_list)
resamps

# Summarize the results
summary(resamps)

# Box-and-whisker
bwplot(resamps)

# Dot plot
dotplot(resamps)

# Density plot
densityplot(resamps)

# Scatter plot
# This plot shows you how similar two models' performances are on different folds.
# It's particularly useful for identifying if one model is consistently better 
# than the other across all folds, or if there are situations when the inferior 
# model produces better predictions on a particular subset of the data.
xyplot(resamps)

# Predict on the testing set
set.seed(62433)
predrfmod <- predict(rfmod,testing)
confusionMatrix(predrfmod,testing$diagnosis)

set.seed(62433)
predgbmmod <- predict(gbmmod,testing)
confusionMatrix(predgbmmod,testing$diagnosis)

set.seed(62433)
predldamod <- predict(ldamod,testing)
confusionMatrix(predldamod,testing$diagnosis)

# Stack the predictions together using random forests ("rf"). 
# What is the resulting accuracy on the test set? 
# Is it better or worse than each of the individual predictions?

# Fit a model that combines predictors
predDF <- data.frame(predrfmod,predgbmmod,predldamod,diagnosis=testing$diagnosis)
set.seed(62433)
combModFit <- train(diagnosis ~.,method="rf",data=predDF)
set.seed(62433)
combPred <- predict(combModFit,predDF)
confusionMatrix(combPred, predDF[["diagnosis"]])


################################################################################

# Load the concrete data.
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)

library(caret)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#  fit a lasso model to predict Compressive Strength
set.seed(233)
model_las <- train(
        CompressiveStrength ~ ., training,
        method = "lasso"
)

# Print model to console
model_las

# Plot results
plot(model_las)
plot(model_las$finalModel, label = TRUE)
# left side of plot (intercept only model-high penalty) has high lambda values, 
# and the right side of plot (full model with no penalty ) has low lambda value.
# Regression coefficients are shrunk from right to left as you increase the 
# strength on the penalty, therefore decrease the complexity of the model.

# Fit glmnet model: model - Try the defaults(3 values of alpha & 3 values of lambda)
set.seed(233)
model_g <- train(
        CompressiveStrength ~ ., training,
        method = "glmnet"
)

# Print model to console
model_g

# Plot results
plot(model_g)
plot(model_g$finalModel, label = TRUE)
plot(model_g$finalModel, label = TRUE, xlim=c(0.95,1), ylim=c(0.1,0.15))
plot(model_g$finalModel, label = TRUE, ylim=c(0,0.1))
plot(model_g$finalModel, label = TRUE, xlim=c(0,0.1),ylim=c(0,0.15))
# left side of plot (intercept only model-high penalty) has high lambda values, 
# and the right side of plot (full model with no penalty ) has low lambda value.
# Regression coefficients are shrunk from right to left as you increase the 
# strength on the penalty, therefore decrease the complexity of the model.

################################################################################

# Load the concrete data.
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# fit a support vector machine using the e1071 package to predict Compressive 
# Strength using the default settings.
library(e1071)
set.seed(325)
modelsvm <- svm(CompressiveStrength ~ . , training)

# Predict on the testing set
predsvm <- predict(modelsvm, testing, interval = "confidence")
plot(testing$CompressiveStrength, pch=16)
points(predsvm, col = "red", pch=4)

# Calculate RMSE
sqrt(mean((predsvm - testing$CompressiveStrength)^2))

################################################################################

# Load the data on the number of visitors to the instructors blog from here:
url_csv <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(url_csv, destfile= "./gaData.csv")

library(lubridate) # For year() function below
dat = read.csv("./gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package to the training time series. 
library(forecast)
fit <- bats(tstrain)
accuracy(fit)

# Then forecast this model for the remaining time points. 
fcast <- forecast(fit, h=235)
plot(fcast)
points(testing$X, testing$visitsTumblr)

# For how many of the testing points is the true value within the 95% prediction 
# interval bounds?

# 95% confidence interval
CI95 <- fcast$upper
# Upper bound of 95% confidence interval
CI95UP <- as.vector(CI95[,2])

# find how many testing values are within CI95UP
vect <- CI95UP-testing$visitsTumblr
count <- length(vect[vect>=0])
(count/nrow(testing))*100 # % testing values within CI95UP


