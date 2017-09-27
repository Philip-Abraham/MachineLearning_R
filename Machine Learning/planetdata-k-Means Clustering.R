planets <- read.csv("./planetdata.csv", header=TRUE)

# Explore the structure of the dataset
str(planets)

# Chop up moon in my_moons
my_planets <- planets[-1]
planet_name <- planets$planet

# Set random seed. Don't remove this line
set.seed(1)

# Group the moons in three clusters
km_planets <- kmeans(my_planets, 3, nstart = 20)

# Compare the actual moon_name to the clustering using table()
table(planet_name,km_planets$cluster)

# Color the points in the plot based on the clusters
plot(period ~ distance, data = my_planets, col=km_planets$cluster)

# log plot
plot(log(period) ~ log(distance), data = my_planets, col=km_planets$cluster)

# add labels to plots, pos=4 puts labels to the right, cex is text size
with(planets,text(log(period) ~ log(distance), labels=planet, pos=4, cex=1))

# Print out the ratio of the WSS to the BSS
km_planets$tot.withinss/km_planets$betweenss
