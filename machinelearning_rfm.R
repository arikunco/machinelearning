# Title       : machinelearning_rfm.R 
# Description : This is an online retail script and its formatting process with RFM method.
# Objective   : To format data (after cleaning)
# Data source : https://archive.ics.uci.edu/ml/datasets/online+retail 


#load library
library(tidyverse)

#load dataset 
df_ori <- read.csv("dataset/online_retail_clean.csv")

#remove negative value
df_ori <- filter(df_ori, frequency>0 & recency>0 & monetary>0)

#encode dataset breakdown into 4 for each variables 
r <- quantile(df_ori$recency,c(0.25,0.5,0.75))
f <- quantile(df_ori$frequency, c(0.25,0.5,0.75))
m <- quantile(df_ori$monetary, c(0.25,0.5,0.75))

df_encode <- df_ori %>% mutate(m_recency=ifelse(recency<as.numeric(r[1]),1,
                                                      ifelse(recency<as.numeric(r[2]),2,
                                                             ifelse(recency<as.numeric(r[3]),3,4))),
                                   m_frequency=ifelse(frequency<as.numeric(f[1]),1,
                                                      ifelse(frequency<as.numeric(f[2]),2,
                                                             ifelse(frequency<as.numeric(f[3]),3,4))),
                                   m_monetary=ifelse(monetary<as.numeric(m[1]),1,
                                                      ifelse(monetary<as.numeric(m[2]),2,
                                                             ifelse(monetary<as.numeric(m[3]),3,4))))

# preview first 6th rows
head(df_encode)
                                  
# Mengeset benih random 
set.seed(1)

# Select encoded data
df_cluster <- df_encode %>% select (CustomerID,m_recency,m_frequency,m_monetary)

# Mengeksplorasi struktur dataset cars
str(df_cluster)
summary(df_cluster)

# Normalized data (terkadang perlu dinormalisasi apabila interval datanya sangat besar)
# df_norm <- as.data.frame(apply(df_cluster[, 2:4], 2, function(x) (x - min(x))/(max(x)-min(x))))

# As it is (tanpa normalisasi, hilangkan customerID)
df_norm <- df_cluster[,c(2:4)]

# Menentukan nilai K (Determine Number of Clusters)
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(df_norm, centers = i, nstart = 20)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


# Mengelompokkan dataset ke dalam empat kategori
km <- kmeans(df_norm, 4)

# Memperlihatkan isi dari cluster
km

# Memperlihatkan titik sentroid cluster
km$centers

# Memperlihatkan cluster
km$cluster

# Inspect the result
summary(km)

# Scatter plot of x
pairs(df_ori[,c(2:4)], col = km$cluster, pch=20)

# final 
final <- data.frame(df_ori,cluster=km$cluster)

# visualisasi boxplot untuk masing-masing rfm
ggplot(final) + geom_boxplot(aes(x=as.factor(cluster),y=recency)) + xlab("cluster")
ggplot(final) + geom_boxplot(aes(x=as.factor(cluster),y=frequency)) + xlab("cluster")
ggplot(final) + geom_boxplot(aes(x=as.factor(cluster),y=monetary)) + xlab("cluster")

# visualisasi histogram untuk recency
ggplot(filter(final,cluster==4)) + geom_histogram(aes(x=recency), bins=10)

# Exercise untuk machine learning 2 (Kamis, 21 Maret 2019)
# gunakan hierarchical clustering technique untuk meng-cluster dataset online_retail_clean.csv 
