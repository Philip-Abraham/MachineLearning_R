## Multivariable Regression

# Read in your data set and assign to the object:
shop_data <- read.table("./sales.txt", header  = TRUE)

# Add a plot: Is linearity plausible?
plot(sales ~ sq_ft, shop_data)
plot(sales ~ size_dist, shop_data)
plot(sales ~ inv, shop_data)

# Build a linear model for net sales based on all other variables: lm_shop
lm_shop <- lm(sales~., data=shop_data)

# Summarize lm_shop
summary(lm_shop)


## Are all predictors relevant?

# Plot the residuals in function of your fitted observations
plot(lm_shop$fitted.values,lm_shop$residuals)

# Make a Q-Q plot of your residual quantiles
qqnorm(lm_shop$residuals, ylab= "Residual Quantiles")

# Summarize your model, are there any irrelevant predictors?
summary(lm_shop)

# There is no clear pattern in your residuals. Moreover, the residual quantiles 
# are approximately on one line. From the small p-values you can conclude that 
# every predictor is important!

# Predict the net sales based on shop_new.
# Read in your data set and assign to the object:
shop_new<- read.table("./shopnew.txt", header  = TRUE)
predict(lm_shop, shop_new)


## Are all predictors relevant? Take 2!

# Let's take a different dataset. In 2002 Brisbane did a study on different 
# chocolate bars and measured its energy per 100 gram, percentage of proteins, 
# percentage of fat and the total size. You're wondering whether energy is related 
# to the other variables. You can find the results in choco_data.
# Your job is to build a multi linear model for the energy based on all other 
# variables and judge its performance.

# Read in your data set and assign to the object:
choco_data<- read.table("./choc.txt", header  = TRUE)

# Add a plot: Linearity plausible?
plot(energy ~ protein, choco_data)
plot(energy ~ fat, choco_data)
plot(energy ~ size, choco_data)

# Build a linear model for the energy based on all other variables: lm_choco
lm_choco <- lm(energy~., data=choco_data)

# Plot the residuals in function of your fitted observations
plot(lm_choco$fitted.values,lm_choco$residuals)

# Make a Q-Q plot of your residual quantiles
qqnorm(lm_choco$residuals, ylab= "Residual Quantiles")

# Summarize lm_choco
summary(lm_choco)
# The predictor "size" is statistically insignificant, it is best to remove it.


