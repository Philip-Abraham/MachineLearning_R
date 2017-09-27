#### Data Pre-Processing With Caret in R

# The caret package in R provides a number of useful data transforms.
# 
# These transforms can be used in two ways.
# 
# Standalone: Transforms can be modeled from training data and applied to multiple 
# datasets. The model of the transform is prepared using the preProcess() function 
# and applied to a dataset using the predict() function.
# Training: Transforms can prepared and applied automatically during model evaluation. 
# Transforms applied during training are prepared using the preProcess() and 
# passed to the train() function via the preProcess argument.
# A number of data preprocessing examples are presented in this section. 
# They are presented using the standalone method, but you can just as easily use 
# the prepared preprocessed model during model training.
# 
# All of the preprocessing examples in this section are for numerical data. 
# Note that the preprocessing function will skip over non-numeric data without error.

## Summary of Transform Methods

# Review a Summary. It is a good idea to summarize your data before and after a 
# transform to understand the effect it had. The summary() function can be very useful.
# Visualize Data. It is also a good idea to visualize the distribution of your 
# data before and after to get a spatial intuition for the effect of the transform.

# Below is a quick summary of all of the transform methods supported in the method 
# argument of the preProcess() function in caret.
# 
# BoxCox: apply a Box-Cox transform, values must be non-zero and positive.
# YeoJohnson: apply a Yeo-Johnson transform, like a BoxCox, but values can be negative.
# expoTrans: apply a power transform like BoxCox and YeoJohnson.
# zv: remove attributes with a zero variance (all the same value).
# nzv: remove attributes with a near zero variance (close to the same value).
# center: subtract mean from values.
# scale: divide values by standard deviation.
# range: normalize values.
# pca: transform data to the principal components.
# ica: transform data to the independent components.
# spatialSign: project data onto a unit circle.
# The following sections will demonstrate some of the more popular methods.


## Standardize
# Combining the scale and center transforms will standardize your data. 
# Attributes will have a mean value of 0 and a standard deviation of 1.

# load libraries
library(caret)
# load the dataset
data(iris)
# summarize data
summary(iris[,1:4])
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(iris[,1:4], method=c("center", "scale"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, iris[,1:4])
# summarize the transformed dataset
summary(transformed)


## Normalize
# Data values can be scaled into the range of [0, 1] which is called normalization.

# load libraries
library(caret)
# load the dataset
data(iris)
# summarize data
summary(iris[,1:4])
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(iris[,1:4], method=c("range"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, iris[,1:4])
# summarize the transformed dataset
summary(transformed)


## Box-Cox Transform
# When an attribute has a Gaussian-like distribution but is shifted, this is called 
# a skew. The distribution of an attribute can be shifted to reduce the skew and 
# make it more Gaussian. The BoxCox transform can perform this operation (assumes 
# all values are positive).

# load libraries
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# summarize pedigree and age
summary(PimaIndiansDiabetes[,7:8])
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(PimaIndiansDiabetes[,7:8], method=c("BoxCox"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, PimaIndiansDiabetes[,7:8])
# summarize the transformed dataset (note pedigree and age)
summary(transformed)
# Notice, we applied the transform to only two attributes that appear to have a skew.


## Yeo-Johnson Transform
# Another power-transform like the Box-Cox transform, but it supports raw values 
# that are equal to zero and negative.

# load libraries
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# summarize pedigree and age
summary(PimaIndiansDiabetes[,7:8])
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(PimaIndiansDiabetes[,7:8], method=c("YeoJohnson"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, PimaIndiansDiabetes[,7:8])
# summarize the transformed dataset (note pedigree and age)
summary(transformed)


## Principal Component Analysis
# Transform the data to the principal components. The transform keeps components 
# above the variance threshold (default=0.95) or the number of components can be 
# specified (pcaComp). The result is attributes that are uncorrelated, useful for 
# algorithms like linear and generalized linear regression.

# load the libraries
library(mlbench)
# load the dataset
data(iris)
# summarize dataset
summary(iris)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(iris, method=c("center", "scale", "pca"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, iris)
# summarize the transformed dataset
summary(transformed)


## Independent Component Analysis
# Transform the data to the independent components. Unlike PCA, ICA retains those 
# components that are independent. You must specify the number of desired 
# independent components with the n.comp argument. Useful for algorithms such as naive bayes.

# load libraries
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# summarize dataset
summary(PimaIndiansDiabetes[,1:8])
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(PimaIndiansDiabetes[,1:8], method=c("center", "scale", "ica"), n.comp=5)
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, PimaIndiansDiabetes[,1:8])
# summarize the transformed dataset
summary(transformed)
