# Title       : machinelearning_houseprice.R 
# Description : This is a house pricing prediction modeling
# Objective   : To predict sales price of a house 
# Data source : https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data

# load library (tidyverse and caret)
library(randomForest)

# load data (have been chosen and cleaned)

# all_1 <- all %>% select(SalePrice,TotalBsmtSF,GrLivArea,FullBath,HalfBath,BsmtFullBath,BsmtHalfBath,GarageCars,OverallQual,YrSold,YearBuilt,YearRemodAdd)
# 
# all_2 <- all_1 %>% transmute(TotalSqFeet=TotalBsmtSF+GrLivArea, TotBathroom=FullBath+(0.5*HalfBath)+BsmtFullBath+(0.5*BsmtHalfBath),OverallQual,GarageCars,Age=YrSold-YearRemodAdd,SalePrice)

data <- read.csv("dataset/house_price_clean.csv")

# load data test 

train <- data[!is.na(data$SalePrice),]

# linear regression training 
linearMod <- lm(SalePrice ~ TotalSqFeet+TotBathroom+OverallQual+GarageCars+Age, data=train)  # build linear regression model on full data

# randomforest training 
randomFor <- randomForest(SalePrice ~ TotalSqFeet+TotBathroom+OverallQual+GarageCars+Age, data = train, ntree=10000, importance = TRUE)

# print the model 
print(linearMod)

# predict 
predict(linearMod, data.frame(TotalSqFeet=1000,
                             TotBathroom=10,
                             OverallQual=3,
                             GarageCars=1,
                             Age=10))

# You can predict test file and submit in kaggle :)

# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data

test <- data[is.na(data$SalePrice),]

# check NA 
summary(test)

plot_missing(test)

test$TotBathroom[is.na(test$TotBathroom)] <- median(test$TotBathroom, na.rm=TRUE)
test$TotalSqFeet[is.na(test$TotalSqFeet)] <- median(test$TotalSqFeet, na.rm=TRUE)
test$GarageCars[is.na(test$GarageCars)] <- median(test$GarageCars, na.rm=TRUE)

result_lm <- predict(linearMod,test[,c(1:5)])
data.frame(test)
test_labels <- read.csv("dataset/test_labels.csv")
submit <- data.frame(Id=test_labels$x, SalePrice=result_lm)
write.csv(submit,"average.csv",row.names = F)

result_rf <- predict(randomFor, test[,c(1:5)], type = "response")
submit <- data.frame(Id=test_labels$x, SalePrice=result_rf)
write.csv(submit,"average_rf.csv",row.names = F)
