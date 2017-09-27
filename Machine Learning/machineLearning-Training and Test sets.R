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

# You've successfully split the entire dataset into a training set and a test set. 
# Now you are ready to build a model on the training set, and to test its predictive 
# ability on a test set.
# You'll want to build a decision tree on the training set, and next assess its 
# predictive power on a set that has not been used for training: the test set.

# Fill in the model that has been learned.
tree <- rpart(Survived ~ ., data = train, method = "class")

# Predict the outcome on the test set with tree: pred
pred <- predict(tree, test, type="class")

# Calculate the confusion matrix: conf
conf <- table(test$Survived, pred)

# Print this confusion matrix
conf

# Assign TP, FN, FP and TN using conf
TP <- conf[2, 2] # this will be 47
FN <- conf[2, 1] # this will be 38
FP <- conf[1, 2] # this will be 5
TN <- conf[1, 1] # this will be 124

# Calculate and print the accuracy: acc
acc <- (TP+TN)/(TP+FN+FP+TN)
acc
# The confusion matrix reveals an accuracy of (47+127)/(47+38+5+124) = 79.91%. 
# This is less than the 81.65% you calculated in the first section of this chapter. 
# However, this is a much more trustworthy estimate of the model's true predictive power.

# Calculate and print out the precision: prec
prec <- TP/(TP + FP)
prec

# Calculate and print out the recall: rec
rec <- TP/(TP + FN)
rec

# Using Cross Validation
# In this exercise, you will fold the dataset 6 times and calculate the accuracy 
# for each fold. The mean of these accuracies forms a more robust estimation of 
# the model's true accuracy of predicting unseen data, because it is less dependent 
# on the choice of training and test sets.

# Initialize the accs vector
accs <- rep(0,6)

for (i in 1:6) {
        # These indices indicate the interval of the test set
        indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
        
        # Exclude them from the train set
        train <- shuffled[-indices,]
        
        # Include them in the test set
        test <- shuffled[indices,]
        
        # A model is learned using each training set
        tree <- rpart(Survived ~ ., train, method = "class")
        
        # Make a prediction on the test set using tree
        pred <- predict(tree, test, type="class")
        
        # Assign the confusion matrix to conf - test$Survived should be on the rows, pred on the columns.
        conf <- table(test$Survived, pred)
        
        # Assign the accuracy of this model to the ith index in accs
        accs[i] <- sum(diag(conf))/sum(conf)
}

# Print out the mean of accs
mean(accs)
# You just programmed a cross validation algorithm. This estimate will 
# be a more robust measure of your accuracy. It will be less susceptible to the 
# randomness of splitting the dataset.
