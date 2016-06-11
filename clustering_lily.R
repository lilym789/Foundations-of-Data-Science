# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))
library(cluster)
library(rattle)
library(NbClust)

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)
View(wine)

View(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine_d<-subset(wine, select = -c(1) )
class(wine_d)
View(wine_d)
ncol(wine_d)
wine_s<-scale(wine_d, scale=TRUE)
class(wine_s)
View(wine_s)
# Convert to matrix
wine_m<-as.matrix(wine_s)
# Convert to vector
wine_v<-as.vector(wine_m)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine_s)

# Exercise 2:
#   * How many clusters does this method suggest?
## Since the bend in the graph suggests the appropriate # of clusters, we can use 3 clusters in this case, 
## as that is where the bend in the graph is.
#   * Why does this method work? What's the intuition behind it?
## We aim to minimize the within groups sum of squares, so that is where the number is among the lowest.
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_s, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
## As we can see from the graph, this method suggests that the best number 
## of clusters is 3. 

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# Set k=3:
k=3
set.seed(1)
class(wine_s)
View(wine_v)
fit.km <- kmeans(wine_v, centers=k, iter.max=100)
plot(col=fit.km$cluster, wine$Type)

## Output segmented image
# turn healthyclusters into a matrix
fitclusters = fit.km$cluster
dim(fitclusters) = c(nrow(wine_s), ncol(wine_s))
image(fitclusters, axes=FALSE, col=rainbow(k))
class(fitclusters)
head(fitclusters)
# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(wine$Type)
table(fit.km$cluster)
# Exercise 6:
# * Visualize these clusters usingfunction clusplot() from the cluster library
# * Would you consider this a good clustering?
View(wine)
wine$Type<-as.numeric(wine$Type)
clusplot(pam(fit.km$cluster, 3))
