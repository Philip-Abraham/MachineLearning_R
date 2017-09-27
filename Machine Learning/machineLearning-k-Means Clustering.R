## k-means: Without Labels

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


## k-means: how well did you do earlier?

# Well, we've found the labels of the seeds. They are stored in the vector seeds_type; 
# there were indeed three types of seeds!
seedlabels <-  read.csv("./seedlabels.csv", header=TRUE)
seeds_type <- seedlabels$labels

# Set random seed. Don't remove this line.
set.seed(100)

# Group the seeds in three clusters using kmeans(). Set nstart to 20 to let R 
# randomly select the centroids 20 times. Assign the result to seeds_km.
seeds_km <- kmeans(seeds, 3, nstart=20)

# Print out seeds_km
seeds_km

# Compare clusters with actual seed types. Set k-means clusters as rows
table(seeds_km$cluster, seeds_type)

# Plot the length as function of width. Color by cluster
plot(seeds$width, seeds$length, col=seeds_km$cluster)
# If you have a look at the table that got generated, you clearly see three groups 
# with 60 elements or more. Overall, you can say that your formed clusters 
# adequately represent adequately the different types of seeds. These larger 
# groups represent the correspondence between the clusters and the actual types. 
# E.g. Cluster 1 corresponds to seed_type 3.


## The influence of starting centroids

# If you call kmeans() without specifying your centroids, R will randomly assign 
# them for you. In this exercise, you will see the influence of these starting 
# centroids yourself using the seeds dataset.
# To compare the clusters of two cluster models, you can again use table(). 
# If every row and every column has one value, the resulting clusters completely 
# overlap. If this is not the case, some objects are placed in different clusters.

# Set random seed. Don't remove this line.
set.seed(100)

# Apply kmeans to seeds twice: seeds_km_1 and seeds_km_2
seeds_km_1 <- kmeans(seeds, 5, nstart=1)
seeds_km_2 <- kmeans(seeds, 5, nstart=1)

# Return the ratio of the within cluster sum of squares
seeds_km_1$tot.withinss/seeds_km_2$tot.withinss

# Compare the resulting clusters
table(seeds_km_1$cluster, seeds_km_2$cluster)
# As you can see, some clusters remained the same, others have changed. 
# For example, cluster 5 from seeds_km_1 completely contains cluster 1 from 
# seeds_km_2 (33 objects). Cluster 4 from seeds_km_1 is split, 18 objects were 
# put in seeds_km_2's fourth cluster and 41 in its fifth cluster. For consistent 
# and decent results, you should set nstart > 1 or determine a prior estimation 
# of your centroids.


## Making a scree plot!
# You're given a dataset school_result containing school level data recording 
# reading and arithmetic scores for each school's 4th and 6th graders. 
school_result <-  read.table("./school.txt", header=TRUE)
# We're wondering if it's possible to define distinct groups of students based 
# on their scores and if so how many groups should we consider?
# Your job is to cluster the schools based on their scores with k-means, 
# for different values of k. On each run, you'll record the ratio of the within 
# cluster sum of squares to the total sum of squares. The scree plot will tell you 
# which k is optimal!

# Set random seed. Don't remove this line.
set.seed(100)

# Explore the structure of your data
str(school_result)

# Initialise ratio_ss 
ratio_ss <- rep(0,7)

# Finish the for-loop. 
for (k in 1:7) {
        
        # Apply k-means to school_result: school_km
        school_km <-  kmeans(school_result, k, nstart=20)
        
        # Save the ratio between of WSS to TSS in kth element of ratio_ss
        ratio_ss[k] <- school_km$tot.withinss/school_km$totss
        
}

# Make a scree plot with type "b" and xlab "k"
plot(ratio_ss, type="b", xlab="k")
# You want to choose k such that your clusters are compact and well separated. 
# However, the ratio_ss keeps decreasing as k increases. Hence, if you were to 
# minimize this ratio as function of k, you'd end up with a cluster for every 
# school, which is a meaningless result. You should choose k such that when 
# increasing it, the impact on ratio_ss is not significant. The elbow in the scree 
# plot will help you identify this turning point.
# k = 3 or 4 will provide meaningful clustering with overall compact and well 
# separated clusters for this school dataset.


## Non-standardized clustering

# In this exercise you'll cluster the countries based on their unstandardized 
# records and calculate Dunn's index. 

run_record <- read.table("./run.txt", header=TRUE)

# Set random seed. Don't remove this line.
set.seed(1)

# Explore your data with str() and summary()
str(run_record)
summary(run_record)
# Cluster run_record using k-means: run_km. 5 clusters, repeat 20 times
run_km <- kmeans(run_record, 5, nstart = 20)

# Plot the 100m as function of the marathon. Color using clusters
plot(run_record$marathon, run_record$X100m, col=run_km$cluster, xlab="marathon", ylab="x100m")

# Calculate Dunn's index: dunn_km. Print it.
library(cluster)
library(clValid)
dunn_km <- dunn(clusters = run_km$cluster, Data =run_record)
dunn_km
# As you can see in the plot, the unstandarized clusters are completely dominated 
# by the marathon records; you can even separate every cluster only based on the 
# marathon records! Moreover Dunn's index seems to be quite low. Compare your 
# results with the standardized version in the next exercise.


## Standardized clustering
# The unstandardized clusters don't produce satisfying results. Let's see if standardizing helps!

# Set random seed. Don't remove this line.
set.seed(1)

# Standardize run_record, transform to a dataframe: run_record_sc
run_record_sc <- as.data.frame(scale(run_record))

# Cluster run_record_sc using k-means: run_km_sc. 5 groups, let R start over 20 times
run_km_sc <- kmeans(run_record_sc, 5, nstart = 20)

# Plot records on 100m as function of the marathon. Color using the clusters in run_km_sc
plot(run_record$marathon, run_record$X100m, col=run_km_sc$cluster, xlab="marathon", ylab="x100m")

# # Compare the unstandardized clusters with standardized in a nice table
table(run_km$cluster, run_km_sc$cluster)

# Calculate Dunn's index: dunn_km_sc. Print it.
dunn_km_sc <- dunn(clusters = run_km_sc$cluster, Data =run_record_sc)
dunn_km_sc
# The plot now shows the influence of the 100m records on the resulting clusters! 
# Dunn's index is clear about it, the standardized clusters are more compact or/and 
# better separated!