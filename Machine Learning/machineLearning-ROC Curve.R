# Creating the ROC curve
# A medium sized dataset about the income of people given a 
# set of features like education, race, sex, and so on.

# R import .data file extension
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
dat <- read.table(url)

# data frame cleaning
dat <- dat[complete.cases(dat),]
dat$V14 <- NULL
dat$V4 <- NULL
dat$V15 <- as.character(dat$V15)
dat$V15 <- replace(dat$V15, dat$V15 == "<=50K", 0)
dat$V15 <- replace(dat$V15, dat$V15 == ">50K", 1)
dat$V15 <- as.integer(dat$V15)
names(dat) <- c("age", "workclass", "fnlwgt","education.num", 
                        "marital.status", "occupation", "relationship", "race",
                        "sex", "capital.gain", "capital.loss", "hours.per.week", "income")       
               
# First, you'll want to split(70/30) the dataset into train and test sets.

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(dat)
shuffled <- dat[sample(n),]

# Split the data in train and test
train_indices <- 1:round(0.7 * n)
train <- shuffled[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test <- shuffled[test_indices, ]


# Print the structure of train and test
str(train)
str(test)

# Set random seed. Don't remove this line
set.seed(1)

# Build a tree on the training set: tree
library(rpart)
tree <- rpart(income ~ ., train, method = "class")

# Predict probability values using the model: all_probs
all_probs <- predict(tree, test, type="prob")

# Print out all_probs
all_probs

# Select second column of all_probs: probs
probs <- all_probs[,2]

# Now that you have the probabilities of every observation in the test set 
# belonging to the positive class (annual income equal or above $50,000), 
# you can build the ROC curve.

# Load the ROCR library
library(ROCR)

# Use prediction() with probs and the true labels of the test set (in the income 
# column of test) to get a prediction object. Assign the result to pred.
pred <- prediction(probs, test[,13])

# Use performance() on pred to get the ROC curve. The second and third argument of 
# this function should be "tpr" and "fpr". These stand for true positive rate and 
# false positive rate, respectively. Assign to result to perf
perf <- performance(pred, "tpr", "fpr")

# Plot this curve
plot(perf)
# A single look at the ROC curve gives you an idea of the classifier's quality. 
# If the curve comes close to the upper-left corner, it's pretty good. 
# To actually quantify "good", you calculate area under the ROC curve - "auc"

# Make a performance object: perf
perf <- performance(pred, "auc")

# Print out the AUC
perf@y.values[[1]]























