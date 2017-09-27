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

# Build a decision tree using the rpart() function of the rpart package. It comes 
# up with possible feature tests and building a tree with the best of these tests.

# Load the rpart, rattle, rpart.plot and RColorBrewer package
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# The tree was trained with the Gini impurity criterion, which rpart() uses by default.
# Build a tree model: tree
tree_g <- rpart(Survived ~ ., data = train, method = "class")

# Draw the decision tree
fancyRpartPlot(tree_g)

# Now you are going to classify the instances that are in the test set
# Predict the values of the test set: pred
pred_g <- predict(tree_g, test, type="class")

# Construct the confusion matrix: conf
conf_g <- table(test$Survived, pred_g)
conf_g

# Print out the accuracy
acc_g <- sum(diag(conf_g))/sum(conf_g)
acc_g
# Around 80 percent of all test instances have been classified correctly. That's not bad!


# Change the training criterion to use information gain as splitting criterion
tree_i <- rpart(Survived ~ ., data = train, method = "class", parms = list(split = "information"))

# Draw the decision tree
fancyRpartPlot(tree_i)

# Now you are going to classify the instances that are in the test set
# Predict the values of the test set: pred
pred_i <- predict(tree_i, test, type="class")

# Construct the confusion matrix: conf
conf_i <- table(test$Survived, pred_i)
conf_i

# Print out the accuracy
acc_i <- sum(diag(conf_i))/sum(conf_i)
acc_i
# Around 80 percent of all test instances have been classified correctly. That's not bad!



#Pruning a complex tree
# Calculation of a complex tree
set.seed(1)
tree <- rpart(Survived ~ ., train, method = "class", control = rpart.control(cp=0.00001))

# Draw the complex tree
fancyRpartPlot(tree)

# Prune the tree: pruned
# The cp argument to be 0.01. This is a complexity parameter. It basically tells 
# the algorithm to remove node splits that do not sufficiently decrease the impurity.
pruned <- prune(tree, cp=0.01)

# Draw pruned
fancyRpartPlot(pruned)
# Another way to check if you overfit your model is by comparing the accuracy on 
# the training set with the accuracy on the test set. You'd see that the difference 
# between those two is smaller for the simpler tree. You can also set the cp 
# argument while learning the tree with rpart() using rpart.control.