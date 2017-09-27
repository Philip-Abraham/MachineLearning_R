# Load the training data:
url_csvtr <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(url_csvtr, destfile= "./pml-training.csv")
dattr = read.csv("./pml-training.csv")

# Load the testing data:
url_csvts <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url_csvts, destfile= "./pml-testing.csv")
datts = read.csv("./pml-testing.csv")


# Parallelizing your code.
# Set up Parallel package for multi-core training
library(parallel)
library(doParallel)
# Calculate the number of cores to use for multi-core training
no_cores <- detectCores() - 1 # convention to leave 1 core for OS
# Initiate cluster
cluster <- makeCluster(no_cores)
registerDoParallel(cluster)


# Data Exploration
str(dattr)
names(dattr)
str(datts)
names(datts)

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


# write a text file of cleaned test dataframe
write.table(datts_rev, row.name = FALSE,col.names = TRUE, file = "dattsREV.txt") #to open as a text file
# write a csv file of cleaned dataframe
write.csv(datts_rev, row.names = FALSE,col.names = TRUE, file = "dattsREV.csv") #to open in excel as csv


# set up training run for x and y syntax, because model format performs poorly
x <- dattr_rev[,-53]
y <- dattr_rev[,53]


# Create train/test indexes
library(caret)
set.seed(42)

# Create Folds
# Leverage caret to create 25 total folds, but ensure that class distributions
# matches the overall training data set. This is known as stratified
# cross validation and generally produces better results.
mymultiFolds <- createMultiFolds(dattr_rev$classe, k = 5, times = 5)

# Compare class distribution in one of the one of 25 folds
i3 <- mymultiFolds$Fold3.Rep3
table(dattr_rev$classe[i3]) / length(i3)

# Summarize the target variable in dattr-rev
table(dattr_rev$classe) / nrow(dattr_rev)

# Use five-fold cross validation for the model
myControl <- trainControl(
        method = "repeatedcv", number = 5, repeats = 5,
        index=mymultiFolds,
        classProbs = TRUE,
        verboseIter = TRUE,
        savePredictions = TRUE,
        allowParallel = TRUE
)

## Random Forest on HAR classe data
# Slower to fit than glmnet
# Less interpretable
# Often (but not always) more accurate than glmnet
# Easier to tune
# Require li$le preprocessing
# Capture threshold effects and variable interactions

set.seed(42)

# The proc.time command essentially works as a stop-watch: you 
# initialize it to a starting time, run all the code desired, 
# and then stop it by subtracting the starting time from the ending time.
# Start the clock!
ptm <- proc.time()


# Train Random Forest model
model_rf <- train(
  x,y,
  metric = "Accuracy",
  method = "ranger",
  importance='impurity', # extract variable importance in ranger
  trControl = myControl
)


# Stop the clock
proc.time() - ptm
# The user time relates to the execution of the code, the system time 
# relates to your CPU, and the elapsed time is the difference in 
# times since you started the stopwatch (and will be equal to the 
# sum of user and system times if the chunk of code was run altogether).


model_rf
# Plot Model
plot(model_rf)
plot(varImp(model_rf))


## De-register parallel processing and Shutdown cluster
stopCluster(cluster)
registerDoSEQ()
