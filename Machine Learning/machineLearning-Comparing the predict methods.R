library(titanic)
data("titanic_train")

titanic <- titanic_train[,c(2,3,5,6)]
titanic <-  titanic[complete.cases(titanic),]

# First, you'll want to split(70/30) the dataset into train and test sets. You'll notice 
# that the titanic dataset is sorted on titanic$Survived , so you'll need to first 
# shuffle the dataset in order to have a fair distribution of the output variable 
# in each set.

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(titanic)
shuffled <- titanic[sample(n),]

# Split the data in train and test
train_indices <- 1:round(0.7 * n)
train <- shuffled[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test <- shuffled[test_indices, ]


# Print the structure of train and test
str(train)
str(test)

### DECISION TREE METHOD ###
# Build a decision tree using the rpart() function of the rpart package. It comes 
# up with possible feature tests and building a tree with the best of these tests.

# Load the rpart
library(rpart)

# The tree was trained with the Gini impurity criterion, which rpart() uses by default.
# Build a tree model: tree
tree_g <- rpart(Survived ~ ., data = train, method = "class")

# Predict probability values using the model: all_probs
all_probs_t <- predict(tree_g, test, type="prob")

# Select second column of all_probs: probs
probs_t <- all_probs_t[,2]

# Now that you have the probabilities of every observation in the test set 
# belonging to the passengers who survived titatanic , 
# you can build the ROC curve.

# Load the ROCR library
library(ROCR)

# Use prediction() with probs and the true labels of the test set (in the income 
# column of test) to get a prediction object. Assign the result to pred.
pred_t <- prediction(probs_t, test[,1])

# Use performance() on pred to get the ROC curve. The second and third argument of 
# this function should be "tpr" and "fpr". These stand for true positive rate and 
# false positive rate, respectively. Assign to result to perf
perf_t <- performance(pred_t, "tpr", "fpr")

# Plot this curve
plot(perf_t)
# A single look at the ROC curve gives you an idea of the classifier's quality. 
# If the curve comes close to the upper-left corner, it's pretty good. 
# To actually quantify "good", you calculate area under the ROC curve - "auc"

# Make a performance object: perf
perf <- performance(pred_t, "auc")

# Print out the AUC
perf@y.values[[1]]