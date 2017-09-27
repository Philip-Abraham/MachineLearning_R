# Your client has provided you with a dataset, crime_data, containing info on the 
# crimes committed in each of the 50 US states and the percentage of urban 
# population. 
crime_data <- read.table("./crime.txt", header=TRUE)

# He'd like you to group the states in 4 clusters. He didn't specify 
# which similarity to use, but the euclidean distance seems acceptable, don't you agree?
# You decide to try out two techniques: k-means and single-linkage hierarchical 
# clustering. You then want to compare the results by calculating the Dunn's 
# indices to make a conclusion. Which clustering will you deliver to your client?

# Set random seed. Don't remove this line.
set.seed(1)

# Scale the dataset: crime_data_sc
crime_data_sc <- scale(crime_data)

# Perform k-means clustering: crime_km
crime_km <-  kmeans(crime_data_sc, 4, nstart=20)

# Perform single-linkage hierarchical clustering
## Calculate the distance matrix: dist_matrix
dist_matrix <- dist(crime_data_sc)

## Calculate the clusters using hclust(): crime_single
crime_single <- hclust(dist_matrix, method="single")

## Cut the clusters using cutree: memb_single
memb_single <-  cutree(crime_single,4)

# Calculate the Dunn's index for both clusterings: dunn_km, dunn_single
dunn_km <- dunn(clusters=crime_km$cluster, Data =crime_data_sc)
dunn_single <- dunn(clusters=memb_single, Data =crime_data_sc)
# Print out the results
dunn_km
dunn_single
