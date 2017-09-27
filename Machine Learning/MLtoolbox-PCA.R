x<-c(2.5,0.5,2.2,1.9,3.1,2.3,2,1,1.5,1.1)
y<-c(2.4,0.7,2.9,2.2,3.0,2.7,1.6,1.1,1.6,0.9)

orgdat <- data.frame(x, y)

# center the data
dat <- scale(orgdat,center=TRUE, scale = TRUE) # Scale should be true if variables are on different scales

# plot centered data
plot(dat)

# calculate the covariance matrix
covmat <- cov(dat)
covmat
# The off-diagonal values are the covariances between variables. They reflect 
# distortions in the data (noise, redundancy, .). Large off-diagonal values 
# correspond to high distortions in our data.
# In the covariance table above, the off-diagonal values are different from zero. 
# This indicates the presence of redundancy in the data. In other words, there is 
# a certain amount of correlation between variables.
# The aim of PCA is to minimize this distortions and to summarize the essential 
# information in the data

# The diagonal elements are the variances of the different variables. 
# A large diagonal values correspond to strong signal
diag(covmat)

# Eigenvalues : The numbers on the diagonal of the diagonalized covariance matrix 
# are called eigenvalues of the covariance matrix. Large eigenvalues correspond to 
# large variances.
# Eigenvectors : The directions of the new rotated axes are called the eigenvectors 
# of the covariance matrix.

# calculate eigenvalues and eigenvectors of the covariance matrix
ev <- eigen(covmat)
ev

eval1 <- ev$values[1] # eigenvalue#1
ev1 <- ev$vectors[,1] # eigenvector#1
eval2 <- ev$values[2] # eigenvalue#2
ev2 <- ev$vectors[,2] # eigenvector#2
# The eigenvector(ev1) with the highest eigenvalue(eval1) is the principle 
# component of the data set.

# The dot product of the eigenvectors of the symmetric covariance matrix is zero
# this means the eigenvectors are orthogonal to each other
ev1%*%ev2

# Compute the new dataset with both eigenvectors retained:
# Keeping both eigenvectors for the transformation, we get the data and
# the plot. This plot is basically the original data, rotated so that the
# eigenvectors are the axes.
        
# Transpose eigenvectors
eigenvectors.t <- t(ev$vectors)
# Transpose the adjusted data
dat.t <- t(dat)
# The new dataset
dat.new <- eigenvectors.t %*% dat.t
# Transpose new data and rename columns
dat.new <- t(dat.new)
colnames(dat.new) <- c("PC1", "PC2")
dat.new # transformed data
plot(dat.new)


# Compute the new dataset with only first(larger eigenvalue) eigenvector retained:
# The other transformation we can make is by taking only the eigenvector with the
# largest eigenvalue. The resulting dataset only has a single dimension. If you 
# compare this data set with the one resulting from using both eigenvectors, you 
# will notice that this data set is exactly the first column of the other. So, if 
# you were to plot this data, it would be 1 dimensional,and would be points on a 
# line in exactly the < positions of the points in the plot created above with both 
# eigenvectors. The single-eigenvector decomposition has removed the contribution
# due to the smaller eigenvector and left us with data that is only in terms of the other.

# Transpose eigenvectors
eigenvectors.t1 <- t(ev1)
# Transpose the adjusted data
dat.t <- t(dat)
# The new dataset
dat.new1 <- eigenvectors.t1 %*% dat.t
# Transpose new data and rename columns
dat.new1 <- t(dat.new1)
colnames(dat.new1) <- c("PC1")
dat.new1 # transformed data
plot(dat.new1,dat.new1)


## This time we will use R's princomp function to perform PCA
pca <- princomp(dat) # since the data is already standardized, the default cor=FALSE
summary(pca)
# As you can see, principal component 1 have the highest standard deviation

# To obtain the actual principal component coordinates ("scores") for each row
pca$scores

# To produce the biplot, a visualization of the principal components against the original variables
biplot(pca)
# The closeness of the x and y arrows indicates that these two variables are, intuitively, correlated.

# plot pca
# The plot method returns a plot of the variances (y-axis) associated with the 
# PCs (x-axis). The Figure below is useful to decide how many PCs to retain for 
# further analysis. In this simple case with only 2 PCs this is not a hard task 
# and we can see that the first PC explain most of the variability in the data.
plot(pca, type = "l")

# creating a plot function
pcaCharts <- function(x) {
        x.var <- x$sdev ^ 2
        x.pvar <- x.var/sum(x.var)
        print("proportions of variance:")
        print(x.pvar)
        
        par(mfrow=c(2,2))
        plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
        plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
        screeplot(x)
        screeplot(x,type="l")
        par(mfrow=c(1,1))
}

# running plot function
pcaCharts(pca)
