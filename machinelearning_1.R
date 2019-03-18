# Title     : Machine Learning I (Regression and Classification)
# Data MBA  : Pertemuan #14
# Tanggal   : 20 Maret 2019

# install Package jika belum
# install.packages("ISLR")
# install.packages("rpart")
# install.packages("randomForest")

# Load library yang sudah diinstall
library(ISLR) # package yang mengeload dataset: Wage
library(rpart) # package yang memuat fungsi rpart (decision tree)
library(randomForest) # package yang memuat fungsi randomForest

# Regresi Linear Sederhana
data.wage <- Wage

str(data.wage)

# Membuat model lm_wage dengan fungsi lm
# Modelnya memprediksi gaji dari umur seseorang 
lm_wage <- lm(wage ~ age, data = Wage)

# Memprediksi gaji dari seseorang yang berumur 25 tahun
unseen <- data.frame(age = 25)
predict(lm_wage,unseen)

# RMSE 
RMSE <- sqrt(c(crossprod(my.lm$residuals))/length(my.lm$residuals))

# RSquare
summary(my.lm)$r.squared

# Multi Linear Regression # Coba exercise dengan dataset airquality, kita ingin memprediksi Temperature, bila diketahui 
# nilai ozone, solar.R, dan Wind. 
airquality <- datasets::airquality[,1:4]

airquality <- na.omit(airquality)

# Training 
lm_airquality <- lm(Temp ~ Ozone + Solar.R + Wind, data=airquality )

# Memprediksi temperatur dalam Farenhite, bila diketahui Ozone, Solar.R, dan Wind
predict.lm(lm_airquality, data.frame(Ozone=25, Solar.R=7, Wind=100))

# Logistic Regression

# Mari kita susun sendiri dataset untuk latihan logistic regression ini          

# Vektor jumlah jam belajar 
jam_belajar = c(1.0,1.5,2.0,2.5,3.0,3.5,3.6,
                4.2,4.5,5.4,6.8,6.9,7.2,7.4,
                8.1,8.2,8.5,9.4,9.5,10.2)

# Vektor lulus atau tidak (0 tidak lulus, 1 lulus)
lulus = c(0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,1,1,1,1,1)

# membentuk data frame data_log dengan variable jam_belajar, dan lulus. 
data_log <- data.frame(jam_belajar=jam_belajar, lulus=lulus)

# mengeplot data_log antara jam_belajar (x axis) dengan lulus (y axis)
plot(data_log$jam_belajar, data_log$lulus,col="black")

# training model with logistic regression
logitMod <- glm(lulus ~ jam_belajar, data=data_log, family=binomial(link="logit"))
summary(logitMod)

# memprediksi apakah seseorang lulus atau tidak (tipe nya response, outputnya berupa probabilitas dari 0 s.d. 1)
# memprediksi 1 observasi
predict.glm(logitMod, data.frame(jam_belajar=8), type = "response")
# memprediksi beberapa observasi 
predict(logitMod, data.frame(data_log$jam_belajar), type = "response")

## Split in train + test set
data_log$lulus <- as.factor(data_log$lulus)
idxs <- sample(1:nrow(data_log),as.integer(0.6*nrow(data_log)))
trainDataLog <- data_log[idxs,]
testDataLog <- data_log[-idxs,]

# Decision Tree 
tree <- rpart(lulus ~ ., 
              data = data.frame(trainDataLog), method = "class")
predict(tree, data.frame(testDataLog),type="class" )
table(testDataLog[,'lulus'],predict(tree, data.frame(testDataLog),type="class"))

# Random forest 
# randomforest training 
randomFor <- randomForest(lulus ~ ., data = data.frame(trainDataLog), ntree=10000, importance = TRUE)
predict(randomFor, data.frame(testDataLog),type="class")
table(testDataLog[,'lulus'],predict(tree, data.frame(testDataLog), type="class"))

# Exercise: Ganti dengan dataset yang lain (misal dataset employee churn rate)
df_employee <- _____.csv("dataset/HR_comma_sep.csv")

# split test and train
df_employee$left <- as.factor(df_employee$left)
urutan <- sample(1:nrow(df_employee),as.integer(0.7*nrow(df_employee)))
trainDataLog <- df_employee[____,]
testDataLog <- df_employee[-_____,]

randomFor <- randomForest(left ~ ., data = data.frame(trainDataLog), ntree=500, importance = TRUE)
predict(randomFor, data.frame(testDataLog),type="class")
table(testDataLog[,'left'],predict(randomFor, data.frame(testDataLog), type="class"))
prop.table(table(testDataLog[,'left'],predict(randomFor, data.frame(testDataLog), type="class")))

# Berapa akurasinya? 
# Coba dengan algoritma decision tree dan logistic regression! 
