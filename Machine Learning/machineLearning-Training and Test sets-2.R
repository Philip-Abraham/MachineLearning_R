# Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

# The following commands will create non-overlapping training and test sets with 
# about 50% of the observations assigned to each.
library(caret)
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]