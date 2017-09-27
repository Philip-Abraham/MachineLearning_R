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

for(i in 1:nrow(train)){
        if(train$Sex[i]=="male"){
                train$Sex[i]=1
        }else{train$Sex[i]=0}
}
train$Sex <- as.numeric(train$Sex)

for(i in 1:nrow(test)){
        if(test$Sex[i]=="male"){
                test$Sex[i]=1
        }else{test$Sex[i]=0}
}
test$Sex <- as.numeric(test$Sex)

# Print the structure of train and test
str(train)
str(test)



# Classify observations, with k-Nearest Neighbors (k-NN)

# Store the Survived column of train and test in train_labels and test_labels
train_labels <- train$Survived
test_labels <- test$Survived

# Copy train and test to knn_train and knn_test
knn_train <- train
knn_test <- test

# Drop Survived column for knn_train and knn_test
knn_train$Survived <- NULL
knn_test$Survived <- NULL

# Normalize Pclass
min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)

# Normalize Age
min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age)
knn_test$Age <- (knn_test$Age - min_age) / (max_age - min_age)

# Classifying some instances with k-Nearest Neighbors
library(class)

# Make predictions using knn: pred
pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 5)

# Construct the confusion matrix: conf
conf <- table(test_labels, pred)

# Print out the confusion matrix
conf

# find accuracy of prediction
accs <- sum(diag(conf))/sum(conf)
accs

# A big issue with k-Nearest Neighbors is the choice of a suitable k. How many 
# neighbors should you use to decide on the label of a new observation? Let's 
# have R answer this question for us and assess the performance of k-Nearest 
# Neighbor classification for increasing values of k.

# Load the class package, define range and accs
library(class)
range <- 1:round(0.2 * nrow(knn_train))
accs <- rep(0, length(range))

for (k in range) {
        
        # Fill in the ___, make predictions using knn: pred
        pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = k)
        
        # Fill in the ___, construct the confusion matrix: conf
        conf <- table(test_labels, pred)
        
        # Fill in the ___, calculate the accuracy and store it in accs[k]
        accs[k] <- sum(diag(conf))/sum(conf)
}

# Plot the accuracies. Title of x-axis is "k".
plot(range, accs, xlab = "k")

# Calculate the best k
which.max(accs)
# For an even more robust estimate of the best k, you could use cross validation