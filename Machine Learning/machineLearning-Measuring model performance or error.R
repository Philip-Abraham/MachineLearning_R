# Measuring model performance or error

library(titanic)
data("titanic_train")

titanic <- titanic_train[,c(2,3,5,6)]
titanic <-  titanic[complete.cases(titanic),]

# Set random seed. Don't remove this line
set.seed(1)

# Have a look at the structure of titanic
str(titanic)

# A decision tree classification model is built on the data
library(rpart) #builds a supervised learning model - Recursive partitioning (a.k.a. decision trees)
tree <- rpart(Survived ~ ., data = titanic, method = "class")

# Use the predict() method to make predictions, assign to pred
pred <- predict(tree, titanic, type="class")

# Use the table() method to make the confusion matrix
conf <- table(titanic$Survived, pred)
# So to summarize, 212 out of all 265 survivors were correctly predicted to have 
# survived. On the other hand, 371 out of the 449 deceased were correctly 
# predicted to have perished.

# Assign TP, FN, FP and TN using conf
TP <- conf[2, 2] # this will be 212
FN <- conf[2, 1] # this will be 78
FP <- conf[1, 2] # this will be 53
TN <- conf[1, 1] # this will be 371

# Calculate and print the accuracy: acc
acc <- (TP+TN)/(TP+FN+FP+TN)
acc

# Calculate and print out the precision: prec
prec <- TP/(TP + FP)
prec

# Calculate and print out the recall: rec
rec <- TP/(TP + FN)
rec


## The quality of a regression

# Imagine this: you're working at NASA and your team measured the sound pressure 
# produced by an airplane's wing under different settings. These settings are the 
# frequency of the wind, the angle of the wing, and several more. The results of 
# this experiment are listed in the air dataset (Source: UCIMLR).
# 
# Your team wants to build a model that's able to predict the sound pressure 
# based on these settings, instead of having to do those tedious experiments every time.
# 
# A colleague has prepared a multivariable linear regression model, fit. It takes 
# as input the predictors: wind frequency (freq), wing's angle (angle), and 
# chord's length (ch_length). The response is the sound pressure (dec). 
# All these variables can be found in air.
# 
# Now, your job is to assess the quality of your colleague's model by calculating the RMSE:

# An easy way to check this if you are unfamiliar with the dataset is to first use 
# readLines to check a few lines, as below:
readLines("https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat", n=10)

# https URL to the airfoil_self_noise file.
url_dat <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat"

# Download the file at url_dat using download.file(). Call the file "air".
download.file(url_dat, destfile= "./air.dat")

# Read dat file (no lines to skip so skip =0)
air <- read.table("./air.dat", header=FALSE)

names(air) <- c("freq", "angle", "ch_length", "velocity", "thickness", "dec")

# Take a look at the structure of air
str(air)

# Inspect your colleague's code to build the model
fit <- lm(dec ~ freq + angle + ch_length, data = air)

# Use the model to predict for all values: pred
pred <- predict(fit)

# Use air$dec and pred to calculate the RMSE 
rmse <- sqrt((1/nrow(air)) * sum( (air$dec - pred) ^ 2))

# Print out rmse
rmse
# So the RMSE is given by  5.215778 what? Well 5 decibels, the unit of the sound 
# pressure, your response variable. As a standalone number, it doesn't tell you a 
# whole bunch. In order to derive its meaning, it should be compared to the RMSE of 
# a different model for the same problem

# Adding the new variables will definitely increase the complexity of your model, 
# but will it increase the performance? To find out, we'll take the RMSE from the 
# new, more complex model and compare it to that of the original model.

# Your colleague's more complex model
fit2 <- lm(dec ~ freq + angle + ch_length + velocity + thickness, data = air)

# Use the model to predict for all values: pred2
pred2 <- predict(fit2)

# Calculate rmse2
rmse2 <- sqrt(sum( (air$dec - pred2) ^ 2) / nrow(air))

# Print out rmse2
rmse2
# Adding complexity seems to have caused the RMSE to decrease, from 5.216 to 4.799. 
# But there's more going on here; perhaps adding more variables to a regression 
# always leads to a decrease of your RMSE? 


#Let's do some clustering! with seeds data which lost all its labels

# In the dataset "seeds" you can find various metrics such as area, perimeter and 
# compactness for 210 seeds. (Source: UCIMLR). However, the seeds' labels were lost. 
# Hence, we don't know which metrics belong to which type of seed. What we do know, 
# is that there were three types of seeds.

seeds <- read.table("./seeds.txt", header=TRUE)

# Set random seed. Don't remove this line
set.seed(1)

# Explore the structure of the dataset
str(seeds)

# Group the seeds in three clusters
km_seeds <- kmeans(seeds, 3)

# Color the points in the plot based on the clusters
plot(length ~ compactness, data = seeds, col=km_seeds$cluster)

# Print out the ratio of the WSS to the BSS
km_seeds$tot.withinss/km_seeds$betweenss
# The within sum of squares is far lower than the between sum of squares. 
# Indicating the clusters are well seperated and overall compact. This is further 
# strengthened by the plot you made, where the clusters you made were visually 
# distinct for these two variables. It's likely that these three clusters represent 
# the three seed types well, even if there's no way to truly verify this.
