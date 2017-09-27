# Load the training data:
url_csvtr <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url_csvtr, destfile= "./pml-training.csv")
dattr = read.csv("./pml-training.csv")

# Load the testing data:
url_csvts <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url_csvts, destfile= "./pml-testing.csv")
datts = read.csv("./pml-testing.csv")

# Data Exploration
str(dattr)
names(dattr)

table(dattr$classe)
plot(table(dattr$classe))

table(dattr$user_name)
table(dattr$user_name,dattr$classe)
table(dattr$raw_timestamp_part_1,dattr$classe)
table(dattr$raw_timestamp_part_2,dattr$classe)
table(dattr$num_window,dattr$classe)

dattime <- data.frame(as.Date(dattr$cvtd_timestamp, "%d/%m/%Y"), dattr$classe)
plot(dattime$as.Date.dattr.cvtd_timestamp....d..m..Y.., dattime$dattr.classe)

# Remove non-impact variables from training and testing datasets and data clean-up
dattr_rev <- subset(dattr, select=-c(1:7,12:36,50:59,69:83,87:101,103:112,125:139,141:150))
dattr_rev[dattr_rev == ""] <- NA # convert blanks to NA
dattr_rev<-subset(dattr_rev, complete.cases(dattr_rev)) # Remove NA's
write.table(dattr_rev, row.name = FALSE,col.names = TRUE, file = "dattrREV.txt") #to open in excel and save as csv

datts_rev <- subset(datts, select=-c(1:7,12:36,50:59,69:83,87:101,103:112,125:139,141:150,160))
datts_rev[datts_rev == ""] <- NA # convert blanks to NA
datts_rev<-subset(datts_rev, complete.cases(datts_rev)) # Remove NA's
write.table(datts_rev, row.name = FALSE,col.names = TRUE, file = "dattsREV.txt") #to open in excel and save as csv

# Summarize the target variable in dattr-rev
table(dattr_rev$classe) / nrow(dattr_rev)

# set up training run for x and y syntax, because model format performs poorly
x <- dattr_rev[,-53]
y <- dattr_rev[,53]

# Parallelizing your code.
# Set up Parallel package for multi-core training
library(parallel)
library(doParallel)
# Calculate the number of cores to use for multi-core training
no_cores <- detectCores() - 1 # convention to leave 1 core for OS
# Initiate cluster
cluster <- makeCluster(no_cores)
registerDoParallel(cluster)


# The proc.time command essentially works as a stop-watch: you 
# initialize it to a starting time, run all the code desired, 
# and then stop it by subtracting the starting time from the ending time.
# Start the clock!
ptm <- proc.time()


# WE will be exploring three different types of predictive models: 
# glmnet,ranger(Random Forest) and gbm, so the first order of business is to create a reusable 
# trainControl object you can use to reliably compare them.

# The data: HAR Classe
# Fit different models and choose the best
# Models must use the same training/test splits
# Create a shared trainControl object

# Create train/test indexes
library(caret)
set.seed(42)

# Create Folds
myFolds <- createFolds(dattr_rev$classe, k = 10)

# Compare class distribution in the ten folds
i1 <- myFolds$Fold01
table(dattr_rev$classe[i1]) / length(i1)

i2 <- myFolds$Fold02
table(dattr_rev$classe[i2]) / length(i2)

i3 <- myFolds$Fold03
table(dattr_rev$classe[i3]) / length(i3)

i4 <- myFolds$Fold04
table(dattr_rev$classe[i4]) / length(i4)

i5 <- myFolds$Fold05
table(dattr_rev$classe[i5]) / length(i5)

i6 <- myFolds$Fold06
table(dattr_rev$classe[i6]) / length(i6)

i7 <- myFolds$Fold07
table(dattr_rev$classe[i7]) / length(i7)

i8 <- myFolds$Fold08
table(dattr_rev$classe[i8]) / length(i8)

i9 <- myFolds$Fold09
table(dattr_rev$classe[i9]) / length(i9)

i10 <- myFolds$Fold10
table(dattr_rev$classe[i10]) / length(i10)

# Use 10-fold cross validation for each model
myControl <- trainControl(
        method = "cv", number = 10,
        index=myFolds,
        classProbs = TRUE,
        verboseIter = TRUE,
        savePredictions = TRUE,
        allowParallel = TRUE
)


## glmnet on HAR classe data
# Now that you have a reusable trainControl object called myControl, you can start 
# fitting different predictive models to your HAR Classe dataset and evaluate their 
# predictive accuracy.
# glmnet, which penalizes linear and logistic regression models on the size and 
# number of coefficients to help prevent overfitting.  glmnet advantages are:
# Linear model with built-in variable selection
# Great baseline model
# Advantages
# Fits quickly
# Ignores noisy variables
# Provides interpretable coefficients

# Fit the glmnet model with default tuning grid
set.seed(42)
ptmglm <- proc.time()
model_glmnet <- train(
        x,y,
        method = "glmnet",
        trControl = myControl
)
glmtime <- proc.time() - ptmglm
model_glmnet
# Plot Model
plot(model_glmnet)
plot(model_glmnet$finalModel, label = TRUE)
plot(varImp(model_glmnet))

## Fit the glmnet model with custom tuning grid
# Make a custom tuning grid
myGrid <- expand.grid(alpha = 0:1,lambda = 0:10/10)

set.seed(42)
ptmglmCT <- proc.time()
model_glmnet_CT <- train(
  x,y,
  method = "glmnet",
  tuneGrid = myGrid,
  trControl = myControl
)
glmCTtime <- proc.time() - ptmglmCT
model_glmnet_CT
# Plot Model
plot(model_glmnet_CT)
plot(model_glmnet_CT$finalModel, label = TRUE)
plot(varImp(model_glmnet_CT))

## Random Forest on HAR classe data
# Slower to fit than glmnet
# Less interpretable
# Often (but not always) more accurate than glmnet
# Easier to tune
# Require li$le preprocessing
# Capture threshold effects and variable interactions

set.seed(42)
ptmrf <- proc.time()
model_rf <- train(
  x,y,
  metric = "Accuracy",
  method = "ranger",
  importance='impurity', # extract variable importance in ranger
  trControl = myControl
)
rftime <- proc.time() - ptmrf
model_rf
# Plot Model
plot(model_rf)
plot(varImp(model_rf))

## Fit a boosted predictor using the "gbm" method
set.seed (42)
ptmgb <- proc.time()
model_gb <- train(
  x,y,
  metric = "Accuracy",
  method = "gbm",
  trControl = myControl
)
gbtime <- proc.time() - ptmgb
model_gb
# Plot Model
plot(model_gb)
plot(varImp(model_gb))

## Comparing models - resamples() on churn data

# Make a list
model_list <- list(
  randfor = model_rf,
  gbm = model_gb,
  glmnet = model_glmnet_CT
)
# Collect resamples from the CV folds
resamps <- resamples(model_list)
resamps

# Summarize the results
summary(resamps)

# Box-and-whisker
bwplot(resamps, metric = "Accuracy")

# Dot plot
dotplot(resamps, metric = "Accuracy")

# Density plot
densityplot(resamps, metric = "Accuracy")

# Scatter plot
# This plot shows you how similar the two models' performances are on different folds.
# It's particularly useful for identifying if one model is consistently better 
# than the other across all folds, or if there are situations when the inferior 
# model produces better predictions on a particular subset of the data.
xyplot(resamps, metric = "Accuracy")

# scatter plot matrix
splom(resamps)
# three model correlation
modelCor(resamps)


## De-register parallel processing and Shutdown cluster
stopCluster(cluster)
registerDoSEQ()


# Stop the clock
proc.time() - ptm
# The user time relates to the execution of the code, the system time 
# relates to your CPU, and the elapsed time is the difference in 
# times since you started the stopwatch (and will be equal to the 
# sum of user and system times if the chunk of code was run altogether). 
glmtime
glmCTtime
rftime
gbtime