# Title       : Evaluating Model Performance
# Description :  Script to apply the technique for evaluating model performance 
# Class       : Data MBA Class #16
# Date        : 26 March 2019

# Install library clValid, rpart, caret jika belum
# install.packages("clValid")
# install.packages("rpart")
# install.packages("caret")
# install.packages("tidyverse")

library(clValid)
library(rpart)
library(caret)
library(tidyverse)

# menggunakan data wage 
# Membuat model lm_wage dengan fungsi lm
# Modelnya memprediksi gaji dari umur seseorang 

lm_wage <- lm(wage ~ age, data=Wage)

pred <- predict(lm_wage)
pred

# Menggunakan Wage$wage untuk menghitung RMSE
rmse <- sqrt((1/nrow(Wage)) * sum( (Wage$wage - pred) ^ 2))
rmse

# hasilnya 40.9 

# Menghitung R square
summary(lm_wage)$r.squared

# Menghitung adjusted-R Square
summary(lm_wage)$adj.r.squared

# Confusion Matrix
# Kita menggunakan data titanic
titanic <- data.frame(datasets::Titanic)

# Melihat struktur data titanic
str(titanic)

# Set benih acak
set.seed(1)

# Model klasifikasi decision tree. 
tree <- rpart(Survived ~ ., data = titanic, method = "class")

# Menggunakan predict untuk memprediksi, dan diassign ke variable pred
pred <- predict(tree,titanic[,c(1,2,3,5)],type="class") # untuk pembelajaran saat ini, menggunakan data training yang sama

# Membuat confusion matrix dengan fungsi table()
table(titanic$Survived,pred)

# Parameter confusion matrix 
# diassign sebagai conf 
conf <- table(titanic$Survived, pred)

# Meng-Assign TP, FN, FP and TN dari conf 
TP <- conf[1, 1] 
FN <- conf[1, 2] 
FP <- conf[2,1] 
TN <- conf[2,2] 

# Menghitung akurasi 
acc <- (TP+TN)/(TP+FN+FP+TN)
acc

# Menghitung presisi
prec <- TP / (TP+FP)
prec

# Menghitung recall
rec <- TP / (TP+FN)
rec

# Bagaimana menghitung specificity? 
specificity <- TN / (TN+FP)
specificity

# Performance Measurement untuk clustering 
# Set random seed. Don't remove this line.
set.seed(1)

# Explore the cars dataset
str(cars)
summary(cars)

# Group the dataset into two clusters: km_cars
km_cars <- kmeans(cars, 2)

# Print out the contents of each cluster
km_cars

# Print out the cluster centroids
km_cars$centers

# total withinss (kecil makin baik) dan betweenss (besar makin baik)
km_cars$tot.withinss
km_cars$betweenss

# Mengukur performa dengan Dunn's index 
# dunn(distance, clusters, Data = NULL, method = "euclidean")

# distance matrix 
Dist <- dist(cars,method="euclidean")

# menghitung dunn index, makin tinggi makin bagus 
dunn(Dist, km_cars$cluster)
