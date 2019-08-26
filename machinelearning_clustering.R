# Title : Machine Learning (Clustering)


#loading iris database
x <- iris

#Berkenalan dulu dengan data (hint: summary(), dim(), plot())
summary(x)

# Create the k-means model: km.out
km.out <- kmeans(x[,1:4], center=3, nstart=10)

km.out_empat <- kmeans(x[,1:4], center=4, nstart=10)

# Inspect the result
summary(km.out)

# Print the cluster membership component of the model
print(km.out$cluster)

# Print the km.out object
print(km.out)

# Scatter plot of x
plot(x[,2:3], col = km.out$cluster,
     main = "k-means with 3 clusters")

# Determine Number of Clusters
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x[,1:4], centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


#Jika ingin memprediksi dengan data baru, masuk cluster yang mana: 
# https://stackoverflow.com/questions/20621250/simple-approach-to-assigning-clusters-for-new-data-after-k-means-clustering 
# You could use the flexclust package, which has an implemented predict method for k-means. 

#Kode untuk Hierarchical Clustering

#loading iris database
x <- iris

#Berkenalan dulu dengan data (hint: summary(), dim(), plot())

# Calculates similarity as Euclidean distance between observations
d <- dist(x[,1:4])

# Returns hierarchical clustering model
hclust.out <- hclust(d = d)

summary(hclust.out)

# Draws a dendrogram
layout(1)
plot(hclust.out)
abline(h = 3.5, col = "red")

# Cut by height h
cutree(hclust.out, h = 6)

# Cut by number of clusters
cutree(hclust.out, k = 3)

# Fitting hierarchical clustering models using different methods
hclust.complete <- hclust(d, method = "complete")
hclust.average <- hclust(d, method = "average")
hclust.single <- hclust(d, method = "single")

plot(hclust.complete)

# Check if scaling is necessary
colMeans(x[,1:4])
apply(x[,1:4], 2, sd)

# Produce new matrix with columns of mean of 0 and sd of 1
scaled_x <- scale(x[,1:4]) # normalization scaling 
df.scaled_x <- data.frame(scaled_x) # ubah jadi data frame 
colMeans(scaled_x)  # check means for each column 
apply(scaled_x, 2, sd) 
apply(scaled_x, 2, max)

hclust.out.scaled <- hclust(dist(scaled_x))
x$label_scaled <- cutree(hclust.out.scaled,k=3)
x$label <- cutree(hclust.out,k=3)

plot(x[,1:4], col = x$label,
     main = "Hierarchical Cluster of Iris")

############# EXERCISE USING HIERARCHICAL CLUSTER FOR RFM ONLINE RETAIL USE CASE #######

# 1. Buka machinelearning_rfm.R, lakukan hierarchical clustering untuk dataset online retail use case. 
# 2. Create Similarity Matrix untuk dataset online retail use case, jadikan file similarity_matrix.R sebagai inspirasi.
