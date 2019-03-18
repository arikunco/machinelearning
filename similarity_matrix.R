# Title       : similarity_matrix.R
# Description : this code is for checking whether the cluster result are good enough. 

rm(list=ls())

set.seed(1)
dat <- iris[,c(1:4)]
dij <- dist(scale(dat, center = TRUE, scale = TRUE))
dij
clust <- hclust(dij, method = "average")
ord <- order(cutree(clust, k = 3))
ord
coph <- cophenetic(clust)
layout(matrix(1:4, ncol = 2))
image(as.matrix(dij)[ord, ord], main = "Original distances")
image(as.matrix(coph)[ord, ord], main = "Cophenetic distances")
image((as.matrix(coph) - as.matrix(dij))[ord, ord], 
      main = "Cophenetic - Original")
plot(coph ~ dij, ylab = "Cophenetic distances", xlab = "Original distances",
     main = "Shepard Plot")

abline(0,1, col = "red")

# original heatmap
heatmap(as.matrix(dij)[ord, ord] )

