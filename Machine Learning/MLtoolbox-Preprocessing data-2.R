library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function 
# from the caret package. Calculate the number of principal components needed to 
# capture 90% of the variance. How many are there?

training_IL <- training[, -1:-57]
training_IL <- training_IL[, -13:-74]

# using the caret package preProcess - thresh is  cutoff for the cumulative 
# percent of variance to be retained by PCA
trans <- preProcess(training_IL, method=c("center", "scale", "pca"), thresh = 0.90)
trans


# Not using caret package
pca <- prcomp(training_IL,center = TRUE,scale. = TRUE)
summary(pca)

# creating a plot function
pcaCharts <- function(x) {
        x.var <- x$sdev ^ 2
        x.pvar <- x.var/sum(x.var)
        print("proportions of variance:")
        print(x.pvar)
        
        par(mfrow=c(2,2))
        plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
        plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
        screeplot(x)
        screeplot(x,type="l")
        par(mfrow=c(1,1))
}

# running plot function
pcaCharts(pca)

# plot pca
# The plot method returns a plot of the variances (y-axis) associated with the 
# PCs (x-axis). The Figure below is useful to decide how many PCs to retain for 
# further analysis. In this simple case with only 10 PCs this is not a hard task 
# and we can see that the first two PCs explain most of the variability in the data.
plot(pca, type = "l")


biplot(pca,choices = 1:2) # choices - which two sets of PCs to plot


library(devtools)
install_github("ggbiplot", "vqv")

# It projects the data on the first two PCs. Other PCs can be chosen through the 
# argument choices of the function. It colors each point according to the 
# AlzheimerDisease diagnosis and draws a Normal contour line with ellipse.prob 
# probability (default to {68\%}) for each group.
library(ggbiplot)
g <- ggbiplot(pca, choices = 1:2, obs.scale = 1, var.scale = 1, # choices - which two sets of PCs to plot
              groups = training$diagnosis, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
# the plot produced by ggbiplot is much better than the one produced by biplot(pca)


# Create a training data set consisting of only the predictors with variable names 
# beginning with IL and the diagnosis. Build two predictive models, one using the 
# predictors as they are and one using PCA with principal components explaining 80% 
# of the variance in the predictors. Use method="glm" in the train function.
# What is the accuracy of each method in the test set? Which is more accurate?

training_IL <- training[, -2:-57]
training_IL <- training_IL[, -14:-75]

# Basic model
set.seed(42)
model_basic <- train(diagnosis~.,
        method = "glm", data=training_IL
)
model_basic$results
model_basic$preProcess

# PCA model
set.seed(42)
ctrl <- trainControl(preProcOptions = list(thresh = 0.80))
model_pca <- train(diagnosis~., 
             data =training_IL, 
             method = "glm",
             preProcess="pca",
             trControl = ctrl)
model_pca$results
model_pca$preProcess


## Load the cement data
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

summary(training)
str(training)

# Feature plot (caret package)
featurePlot(x=training[,c("Cement","BlastFurnaceSlag","FlyAsh", "Water", 
                          "Superplasticizer", "CoarseAggregate",  "FineAggregate",
                          "Age")],
            y = training$CompressiveStrength,
            plot="pairs")

# Qplot (ggplot2 package)
library(ggplot2)
qplot(FlyAsh, CompressiveStrength, data=training)
qplot(FlyAsh, CompressiveStrength, colour=Cement, data=training)
qplot(FlyAsh, CompressiveStrength, colour=BlastFurnaceSlag ,data=training)
qplot(FlyAsh, CompressiveStrength, colour=Water ,data=training)
qplot(FlyAsh, CompressiveStrength, colour=Superplasticizer ,data=training)
qplot(FlyAsh, CompressiveStrength, colour=CoarseAggregate ,data=training)
qplot(FlyAsh, CompressiveStrength, colour=FineAggregate ,data=training)
qplot(FlyAsh, CompressiveStrength, colour=Age ,data=training)

# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
# Color by each of the variables in the data set (you may find the cut2() function 
# in the Hmisc package useful for turning continuous covariates into factors). 
# What do you notice in these plots?

# cut2, making factors (Hmisc package)
library(Hmisc)

cutCement <- cut2(training$Cement,g=3)
table(cutCement)
qplot(inTrain, CompressiveStrength,colour=cutCement,data=training)

cutBlastFurnaceSlag <- cut2(training$BlastFurnaceSlag,g=3)
table(cutBlastFurnaceSlag)
qplot(inTrain, CompressiveStrength,colour=cutBlastFurnaceSlag,data=training)

cutFlyAsh <- cut2(training$FlyAsh,g=3)
table(cutFlyAsh)
qplot(inTrain, CompressiveStrength,colour=cutFlyAsh,data=training)

cutWater <- cut2(training$Water,g=3)
table(cutWater)
qplot(inTrain, CompressiveStrength,colour=cutWater,data=training)

cutSuperplasticizer <- cut2(training$Superplasticizer,g=3)
table(cutSuperplasticizer)
qplot(inTrain, CompressiveStrength,colour=cutSuperplasticizer,data=training)

cutCoarseAggregate <- cut2(training$CoarseAggregate,g=3)
table(cutCoarseAggregate)
qplot(inTrain, CompressiveStrength,colour=cutCoarseAggregate,data=training)

cutFineAggregate <- cut2(training$FineAggregate,g=3)
table(cutFineAggregate)
qplot(inTrain, CompressiveStrength,colour=cutFineAggregate,data=training)

cutAge <- cut2(training$Age,g=3)
table(cutAge)
qplot(inTrain, CompressiveStrength,colour=cutAge,data=training)
# There is a non-random pattern in the plot of the outcome versus index that is 
# indcating a missing predictor variable
# Add regression smoothers (ggplot2 package)
qq <- qplot(inTrain, CompressiveStrength,colour=cutAge,data=training)
qq +  geom_smooth(method='lm',formula=y~x)

# Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally 
# you might use the log transform to try to make the data more symmetric. Why would 
# that be a poor choice for this variable?
hist(training$Superplasticizer)
summary(training$Superplasticizer)

logtr <- log(training$Superplasticizer)
hist(logtr)
