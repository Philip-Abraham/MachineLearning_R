## Classification: Filtering spam

# Filtering spam from relevant emails is a typical machine learning task. 
# Information such as word frequency, character frequency and the amount of capital 
# letters can indicate whether an email is spam or not.

# Import the file using read.csv(): emails
emails_small <- read.csv("./emails_small.csv", header  = TRUE)

# Show the dimensions of emails
dim(emails_small)

# Inspect definition of spam_classifier()
spam_classifier <- function(x){
        prediction <- rep(NA, length(x)) # initialize prediction vector
        prediction[x > 4] <- 1
        prediction[x >= 3 & x <= 4] <- 0
        prediction[x >= 2.2 & x < 3] <- 1
        prediction[x >= 1.4 & x < 2.2] <- 0
        prediction[x > 1.25 & x < 1.4] <- 1
        prediction[x <= 1.25] <- 0
        return(prediction) # prediction is either 0 or 1
}

# Apply the classifier to the avg_capital_seq column: spam_pred
spam_pred <- spam_classifier(emails_small[,1])

# Compare spam_pred to emails$spam. Use ==
spam_pred == emails_small$spam

# Overfitting the spam!
# Now verify whether the spam_classifier() that was built generalizes to the 
# entire set of emails. The accuracy for the set emails_small was equal to 1. 
# Is the accuracy for the entire set emails_full substantially lower?
# Import the file using read.csv(): emails
emails_full <- read.csv("./emails_full.csv", header  = TRUE)

spam_classifier <- function(x){
        prediction <- rep(NA, length(x)) # initialize prediction vector
        prediction[x > 4] <- 1 
        prediction[x >= 3 & x <= 4] <- 0
        prediction[x >= 2.2 & x < 3] <- 1
        prediction[x >= 1.4 & x < 2.2] <- 0
        prediction[x > 1.25 & x < 1.4] <- 1
        prediction[x <= 1.25] <- 0
        return(factor(prediction, levels = c("0", "1"))) # prediction is either 0 or 1
}

# Apply spam_classifier to emails_full: pred_full
pred_full <- spam_classifier(emails_full[,1])

# Build confusion matrix for emails_full: conf_full
conf_full <- table(emails_full$spam,pred_full)
conf_full
# Calculate the accuracy with conf_full: acc_full
acc_full <- sum(diag(conf_full))/sum(conf_full)

# Print acc_full
acc_full
# This hard-coded classifier gave you an accuracy of around 65% on the full dataset, 
# which is way worse than the 100% you had on the emails_small dataset. 
# Hence, the classifier does not generalize well at all!


# Increasing the bias
# The spam_classifier() from above examples is bogus. It simply overfits on the 
# emails_small set and, as a result, doesn't generalize to larger datasets such as emails_full.
# So let's try something else. On average, emails with a high frequency of 
# sequential capital letters are spam. What if you simply filtered spam based on 
# one threshold for avg_capital_seq?
# 
# For example, you could filter all emails with avg_capital_seq > 4 as spam. 
# By doing this, you increase the interpretability of the classifier and restrict 
# its complexity. However, this increases the bias, i.e. the error due to 
# restricting your model.

# The all-knowing classifier that has been learned for you
# You should change the code of the classifier, simplifying it
spam_classifier <- function(x){
        prediction <- rep(NA, length(x))
        prediction[x > 4] <- 1
        prediction[x <= 4] <- 0
        return(factor(prediction, levels = c("0", "1")))
}

# conf_small and acc_small have been calculated for you
conf_small <- table(emails_small$spam, spam_classifier(emails_small$avg_capital_seq))
conf_small
acc_small <- sum(diag(conf_small)) / sum(conf_small)
acc_small

# Apply spam_classifier to emails_full and calculate the confusion matrix: conf_full
conf_full <- table(emails_full$spam, spam_classifier(emails_full$avg_capital_seq))
conf_full

# Calculate acc_full
acc_full <- sum(diag(conf_full)) / sum(conf_full)

# Print acc_full
acc_full
# Your model no longer fits the small dataset perfectly but it fits the big 
# dataset better. You increased the bias on the model and caused it to generalize 
# better over the complete dataset. While the first classifier overfits the data, 
# an accuracy of 73% is far from satisfying for a spam filter. 
