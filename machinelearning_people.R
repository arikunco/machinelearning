# Title       : machinelearning_people.R 
# Description : This is an online retail script and its formatting process with RFM method.
# Objective   : To format data (after cleaning)
# Data source : https://www.kaggle.com/dguo456/employee-turnover-prediction-logistic-regression/data 

# remove all variables (start from scratch)
rm(list=ls())

# Load library
library(tidyverse)
library(DataExplorer)

# Load dataset 
df_people <- read.csv("dataset/HR_comma_sep.csv",stringsAsFactors = T)

levels(df_people$sales)
levels(df_people$salary) <- c("low","medium","high")
levels(df_people$salary)

# Preview first six rows
head(df_people)

# Preview last six rows 
___(df_people) 

# Column Names 
____(df_people)

# Data type per columns 
___(df_people)

# Statistic Description
_____(df_people)

# check missing value 
plot______(df_people)

# Visualize data 

# barchart of Salary by Left 
counts <- table(df_people[,c('left','salary')])
barplot(counts, main="Salary Distribution vs Left",
        xlab="Salary", col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)

# stackbarchart of Salary by Left
propcount <- prop.table(counts)
barplot(propcount, main="Salary Distribution vs Left",
        xlab="Salary", col=c("darkblue","red"),legend = rownames(counts))

# histogram of satisfaction level by Left 
df_people$left <- as.factor(df_people$left)
ggplot(df_people) + geom_histogram(aes(x=satisfaction_level,color=left), fill="White",alpha=0.5, position="identity")


# Table crosstab of promotion by left 
counts2 <- table(df_people$promotion_last_5years, df_people$left)
p.div(q.sum(0), axis = 0).plot(kind='bar', stacked = True)
barplot(counts2, main="Promotion last 5 years vs Left",
        xlab="Promotion last 5 years", legend = rownames(counts))

# create dummy variables 
df_people_dummy <- dummify(df_people)
plot_missing(df_people_dummy)

# train data
logitMod <- glm(left ~ satisfaction_level + salary_high + salary_medium + promotion_last_5years, data=df_people_dummy, family=binomial(link="logit"))

summary(logitMod)

# predict with new data
predict.glm(logitMod,data.frame(satisfaction_level=0.01,
                                salary_high=1,
                                salary_medium=0,
                                promotion_last_5years=1),
            type='response')

# Exercise 

# Perform training on 70% of data with logistic regression above and the rest (30%) for testing. How is the testing result? How many are correctly predicted? 

# Hint: 
# set.seed(101) # Set Seed so that same sample can be reproduced in future also 
# Now Selecting 50% of data as sample from total 'n' rows of the data 
# sample <- sample.int(n = nrow(data), size = floor(.50*nrow(data)), replace = F) 
# train <- data[sample, ] 
# test <- data[-sample, ]


# Answer: 
  
