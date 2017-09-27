moons <- read.table("./moondata.txt", header=TRUE)

# Explore the structure of the dataset
str(moons)

# Chop up moon in my_moons
rownames(moons) <- moons$moon
my_moons <- moons[-1]
moon_name <- moons$moon

# Set random seed. Don't remove this line
set.seed(1)

## k-means clustering
# Group the moons in three clusters
km_moons <- kmeans(my_moons, 3, nstart = 20)

# Compare the actual moon_name to the clustering using table()
table(moon_name,km_moons$cluster)

# Color the points in the plot based on the clusters
plot(period ~ distance, data = my_moons, col=km_moons$cluster)

# log plot
plot(log(period) ~ log(distance), data = my_moons, col=km_moons$cluster)

# add labels to plots, pos=4 puts labels to the right, cex is text size
with(moons,text(log(period) ~ log(distance), labels=moon, pos=4, cex=1))

# Print out the ratio of the WSS to the BSS
km_moons$tot.withinss/km_moons$betweenss

# Calculate Dunn's index
library(cluster)
library(clValid)
dunn_km_moons <- dunn(clusters = km_moons$cluster, Data =my_moons)
dunn_km_moons


## Compare with Standardized clustering

# Set random seed. Don't remove this line.
set.seed(1)

# Standardize run_record, transform to a dataframe
my_moons_sc <- as.data.frame(scale(my_moons))

# Cluster into 3 groups, let R start over 20 times
km_moons_sc <- kmeans(my_moons_sc, 3, nstart = 20)

# Color the points in the plot based on the clusters
plot(period ~ distance, data = my_moons, col=km_moons_sc$cluster)

# Compare the unstandardized clusters with standardized in a nice table
table(km_moons$cluster, km_moons_sc$cluster)

# Calculate Dunn's index
library(cluster)
library(clValid)
dunn_km_moons_sc <- dunn(clusters = km_moons_sc$cluster, Data =my_moons_sc)
dunn_km_moons_sc
# the standardizwd data dunn index actually in this case is lower than the unstandardized data dunn index.
# therefore it may not be necessary to standardize the dataset.??


# Complete-Linkage Hierarchical Clustering

# Apply dist() to my_moons_sc
moon_dist <- dist(my_moons_sc)

# Apply hclust() to moon_dist
library(stats)
moon_complete <- hclust(moon_dist, method = "complete")

# Apply cutree() to moon_complete: memb_complete
memb_complete <- cutree(moon_complete, 3)

# Apply plot() on moon_complete to draw the dendrogram
plot(moon_complete)

# Apply rect.hclust() on run_complete to draw the boxes
rect.hclust(moon_complete, k = 3, border = 2:4)