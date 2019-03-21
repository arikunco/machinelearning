# Title       : similarity_matrix_v2.R
# Description : this code is for checking whether the cluster result are good enough. 
# Adapted from: https://stackoverflow.com/questions/5639794/in-r-how-can-i-plot-a-similarity-matrix-like-a-block-graph-after-clustering-d

# menghilangkan seluruh variable di global environment
rm(list=ls())

# mengeset benih random 
set.seed(1)

# mengambil dataset iris dari kolom 1 s.d. 4 
dat <- iris[,c(1:4)]

# mengkomputasi matriks dissimilarity dengan jarak Euclidean
dij <- dist(scale(dat))

# membuat hierarchical clustering dengan method average
clust <- hclust(dij, method = "average")
ord <- order(cutree(clust, k = 3))
ord

abline(0,1, col = "red")

# original heatmap
heatmap(as.matrix(dij)[ord, ord] )

coph <- cophenetic(clust)
layout(matrix(1:4, ncol = 2))

#Here are 3 image plots of:
  
#The original dissimilarity matrix, sorted on basis of cluster analysis groupings,
image(as.matrix(dij)[ord, ord], main = "Original distances")

#The cophenetic distances, again sorted as above
image(as.matrix(coph)[ord, ord], main = "Cophenetic distances")

#The difference between the original dissimilarities and the cophenetic distances
image((as.matrix(coph) - as.matrix(dij))[ord, ord], 
      main = "Cophenetic - Original")

#A Shepard-like plot comparing the original and cophenetic distances; the better the clustering at capturing the original distances the closer to the 1:1 line the points will lie
plot(coph ~ dij, ylab = "Cophenetic distances", xlab = "Original distances",
     main = "Shepard Plot")


