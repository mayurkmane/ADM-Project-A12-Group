#setwd("E:/kaggle data/bike sharing")

# We have read train and test data 
train <- read.csv("train_bike.csv")
test <- read.csv("test_bike.csv")

str(train)

#converted each require feature of train data into factor
factor_train <- train
factor_train$weather <- factor(train$weather)
factor_train$holiday <- factor(train$holiday)
factor_train$workingday <- factor(train$workingday)
factor_train$season <- factor(train$season)

#converted each require feature of test data into factor
factor_test <- test
factor_test$weather <- factor(test$weather)
factor_test$holiday <- factor(test$holiday)
factor_test$workingday <- factor(test$workingday)
factor_test$season <- factor(test$season)

#Added time column from datetime
factor_train$time <- substring(train$datetime,12,20)
factor_test$time <- substring(test$datetime,12,20)


#factorized new time column
factor_train$time <- factor(factor_train$time)
factor_test$time <- factor(factor_test$time)
str(factor_train)

#created day of week column from datetime
factor_train$day <- weekdays(as.Date(factor_train$datetime))
factor_train$day <- as.factor(factor_train$day)
factor_test$day <- weekdays(as.Date(factor_test$datetime))
factor_test$day <- as.factor(factor_test$day)

aggregate(factor_train[,"count"],list(factor_train$day),mean)

#In train and test data created Sunday variable 
factor_train$sunday[factor_train$day == "Sunday"] <- "1"
factor_train$sunday[factor_train$day != "Sunday"] <- "0"

factor_test$sunday[factor_test$day == "Sunday"] <- "1"
factor_test$sunday[factor_test$day != "Sunday"] <- "0"

#Factorized sunday variable
factor_train$sunday <- as.factor(factor_train$sunday)
factor_test$sunday <- as.factor(factor_test$sunday)

#To evaluated, convert time and create hour as an integer
factor_train$hour<- as.numeric(substr(factor_train$time,1,2))
factor_test$hour<- as.numeric(substr(factor_test$time,1,2))


#Daypartitioned for 4AM - 9AM = 1
factor_train$daypartition[(factor_train$hour < 10) & (factor_train$hour > 3)] <- 1
factor_test$daypartition[(factor_test$hour < 10) & (factor_test$hour > 3)] <- 1


#Daypartitioned for 10AM - 3PM = 2
factor_train$daypartition[(factor_train$hour < 16) & (factor_train$hour > 9)] <- 2
factor_test$daypartition[(factor_test$hour < 16) & (factor_test$hour > 9)] <- 2


#Daypartitioned for 4PM - 9PM = 3
factor_train$daypartition[(factor_train$hour < 22) & (factor_train$hour > 15)] <- 3
factor_test$daypartition[(factor_test$hour < 22) & (factor_test$hour > 15)] <- 3

#Daypartitioned to factor
factor_train$daypartition <- as.factor(factor_train$daypartition)
factor_test$daypartition <- as.factor(factor_test$daypartition)

#Factorized hour in test and train data
factor_train$hour <- as.factor(factor_train$hour)
factor_test$hour <- as.factor(factor_test$hour)

#install.packages("party")
library('party')

#formula for condition tree
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypartition + sunday

#insert formula into ctree
ctree.fit <- ctree(formula, data=factor_train)
ctree.fit

#applied model on test data
ctree.predict <- predict(ctree.fit, factor_test)

#build a dataframe with our results
output.ctree <- data.frame(datetime = test$datetime, count=ctree.predict)

#written results to .csv for submission
write.csv(output.ctree, file="output_ctree.csv",row.names=FALSE)

