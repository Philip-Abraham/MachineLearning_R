library(datasets)
data(iris)

# Clustering
# Set random seed. Don't remove this line.
set.seed(1)

# Chop up iris in my_iris and species
my_iris <- iris[-5]
species <- iris$Species

# Perform k-means clustering on my_iris: kmeans_iris - # 3 clusters
kmeans_iris <- kmeans(my_iris, 3)

# Compare the actual Species to the clustering using table()
table(species,kmeans_iris$cluster)

# Plot Petal.Width against Petal.Length, coloring by cluster
plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)


# Supervised Learning
library(rpart) #builds a supervised learning model - Recursive partitioning (a.k.a. decision trees) 

# A decision tree model
tree <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
              data = iris, method = "class")

# A dataframe containing unseen observations
unseen <- data.frame(Sepal.Length = c(5.3, 7.2),
                     Sepal.Width = c(2.9, 3.9),
                     Petal.Length = c(1.7, 5.4),
                     Petal.Width = c(0.8, 2.3))

# Predict the label of the unseen observations. Print out the result.
predict(tree, unseen, type = "class")


# Unsupervised Learning
data(mtcars)
cars <- subset(mtcars, select = c(wt,hp))

# Set random seed. Don't remove this line.
set.seed(1)

# Explore the cars dataset
str(cars)
summary(cars)

# Group the dataset into two clusters: km_cars
km_cars <- kmeans(cars,2)

# Print out the contents of each cluster
km_cars$cluster

# Add code: color the points in the plot based on the clusters
plot(cars, col=km_cars$cluster)

# Print out the cluster centroids
km_cars$centers

# Add the centroids to the plot
points(km_cars$centers, pch = 22, bg = c(1, 2), cex = 2)