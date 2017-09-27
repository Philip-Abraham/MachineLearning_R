## Single Hierarchical Clustering

# In this exercise, you'll apply the single hierarchical method to cluster the countries. 

run_record <- read.table("./run.txt", header=TRUE)

# Explore your data with str() and summary()
str(run_record)
summary(run_record)

# Set random seed. Don't remove this line.
set.seed(1)

# Standardize run_record, transform to a dataframe: run_record_sc
run_record_sc <- as.data.frame(scale(run_record))

# Apply dist() to run_record_sc: run_dist
run_dist <- dist(run_record_sc)

# Apply hclust() to run_dist: run_single
library(stats)
run_single <- hclust(run_dist, method="single")

# Apply cutree() to run_single: memb_single
memb_single <- cutree(run_single,5)

# Apply plot() on run_single to draw the dendrogram
plot(run_single)

# Draw boxes around the 5 clusters using rect.hclust(). Set the border argument 
# to 2:6, for different colors.
rect.hclust(run_single, k=5, border = 2:6)
# However, it appears the two islands Samoa and Cook's Islands, who are not known 
# for their sports performances, have both been placed in their own groups. Maybe, 
# we're dealing with some chaining issues? Let's try a different linkage method in 
# the next exercise!


## Complete Hierarchical Clustering

# The clusters of the last exercise weren't truly satisfying. The single-linkage 
# method appears to be placing each outlier in its own cluster. Let's see if 
# complete-linkage agrees with this clustering!

# Apply hclust() to run_dist: run_complete
library(stats)
run_complete <- hclust(run_dist, method = "complete")

# Apply cutree() to run_complete: memb_complete
memb_complete <- cutree(run_complete, 5)

# Apply plot() on run_complete to draw the dendrogram
plot(run_complete)

# Apply rect.hclust() on run_complete to draw the boxes
rect.hclust(run_complete, k = 5, border = 2:6)

# table() the clusters memb_single and memb_complete. Put memb_single in the rows
table(memb_single, memb_complete)

# Comparing the two plots. The five clusters differ significantly from the 
# single-linkage clusters. That one big cluster you had before, is now split up 
# into 4 medium sized clusters. 