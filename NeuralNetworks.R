#setwd("E:/kaggle data/bike sharing")

#install.packages("caret")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("neuralnet")

library(ggplot2)
library(lubridate)
library(neuralnet)
library(caret)

# We have read train and test data 
train <- read.csv("train_bike.csv")
test <- read.csv("test_bike.csv")

set.seed(1)

# Checked is there any data point is missing
apply(train,2,function(x) sum(is.na(x)))

#Extracting require variables and adding two extra variables
extractfeature_traintures <- function(data) {
  features <- c("season",
                "holiday",
                "workingday",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed",
                "hour",
                "count")
  data$hour=substr(data$datetime,12,13)
  data$count <- data$count
  return(data[,features])
}

# Using above function making train data as feature_train
feature_train <- extractfeature_traintures(train)

# Converting the data frame feature_train to numeric matrix for further evaluation
feature_trainN <- data.matrix(feature_train)

# Now dividing data into data into test and 75% train data
index <- sample(1:nrow(feature_trainN),round(0.75*nrow(feature_trainN)))

#Doing normalization before applying neural networks because the algorithm will not converge before the number of maximum iterations allowed.
#Using Min-max method to scale the data in the interval [0,1].Usually scaling in the intervals [0,1] or [-1,1] tends to give better outputs.
maxs <- apply(feature_trainN, 2, max)
mins <- apply(feature_trainN, 2, min)

# scale returns a matrix that needs to be forced into a data.frame
measured <- as.data.frame(scale(feature_trainN, center = mins, scale = maxs - mins))
feature_trainmodel <- measured[index,]
Feature_Model_Test <- measured[-index,]

#Building the neural network. 7 inputs given to input layer, whereas 10 neurons used to build one hidden layer.
#After executing this slow and easy rule, we have received very less SSE(error).
#Generally, in neural network the count of neurons should be 2/3rd of inputs.
#Here we have used threshold as 0.03 and learning rate 0.001 and applied logistic function with resilient back propogation.
exmp <- neuralnet(count~ season + workingday + weather + atemp + humidity + windspeed + hour,data=feature_trainmodel,hidden=8,threshold=.03,stepmax=1e+06,learningrate=.001,algorithm="rprop+",lifesign="full",likelihood=T,act.fct = "logistic",linear.output = FALSE)
plot(exmp)

#We have seen total 51027 iterations with SSE(error) which is quite good.
#We cannot say much about exmpting because net is essentially black box.
#Now model is ready to use due to its significance of convergence.
test_Temp <- subset(Feature_Model_Test, select=c("season","workingday","weather","atemp","humidity","windspeed","hour"))

#Output will give us predicting count
outputs <- compute(exmp,test_Temp)

#Moreover, we need to scale output to do normalized prediction
OrgPr <- (outputs$net.result)*(max(feature_train$count)-min(feature_train$count))+min(feature_train$count)
OrgTest <- (Feature_Model_Test$count)*(max(feature_train$count)-min(feature_train$count))+min(feature_train$count)
frame_result <- data.frame(actual=OrgTest, prediction=OrgPr)
frame_result$prediction = round(frame_result$prediction)

#RMSE and Corelation techniues are used to evaluate prediction accuracy
rmse=sqrt(mean((Feature_Model_Test$count-outputs$net.result)^2))
rmse=sqrt(mean((OrgTest-OrgPr)^2))
cor(OrgTest,OrgPr)

# we got 75.457 rmse value

cor(OrgTest,OrgPr)
#we got correlation value is 0.9060773622

#To get real verses predicted values we have used plot function shown below.
plot(OrgTest,OrgPr,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')


