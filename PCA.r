setwd("D:/PROJECT/ADM/RCode")
bike_train_set <- read.csv("train_bike.csv")
bike_test_set<- read.csv("test_bike.csv")

# Display columns names
bike_train_set.name<-names(bike_train_set)
print(bike_train_set.name)
bike_train_set.rows<-nrow(bike_train_set)
print(paste0('Rows :',bike_train_set.rows))
# columns in train data
bike_train_set.cols<-ncol(bike_train_set)
print(paste0('Columns :',bike_train_set.cols))
# Checking for missing,Na values
table(is.na(bike_train_set))
table(is.na(bike_test_set))
# converting to factors for train
bike_train_set$season<-as.factor(bike_train_set$season)
bike_train_set$holiday<-as.factor(bike_train_set$holiday)
bike_train_set$workingday<-as.factor(bike_train_set$workingday)
bike_train_set$weather<-as.factor(bike_train_set$weather)

# Converting factors for Test
bike_test_set$season<-as.factor(bike_test_set$season)
bike_test_set$holiday<-as.factor(bike_test_set$holiday)
bike_test_set$workingday<-as.factor(bike_test_set$workingday)
bike_test_set$weather<-as.factor(bike_test_set$weather)
head(bike_train_set)
# taking random 2000 rows in train data
bike_train_set.reduced<-bike_train_set[sample(nrow(bike_train_set),2000),]
bike_test_set.reduced<-bike_test_set[sample(nrow(bike_test_set),2000),]

#Display dimensions
dim(bike_train_set.reduced)
# Dividing datetime column
extractdatetime<-function(x){
  x$Year<-substr(x$datetime,1,4)
  x$Month<-substr(x$datetime,6,7)
  x$Day<-substr(x$datetime,9,10)
  x$Hour<-substr(x$datetime,12,13)
  x$Minutes<-substr(x$datetime,15,16)
  x$Second<-substr(x$datetime,18,19)
  return(x)
}
# extracting casual,registered and count.
bike_train_set.reduced<-data.frame(extractdatetime(bike_train_set.reduced))[c(13,14,15,16,17,18,2,3,4,5,6,7,8,9,10,11,12)]
bike_train_set.reducedHour<-as.factor(bike_train_set.reduced$Hour)
par(mfrow=c(1,1))
#Plotting Count vs Hour
boxplot(bike_train_set.reduced$count~bike_train_set.reducedHour,xlab="hour", ylab="Users COunt")
head(bike_train_set.reduced)

install.packages("corrplot")
library(corrplot)

bike_train_set.cor<-data.frame(bike_train_set$temp,bike_train_set$atemp,bike_train_set$windspeed,bike_train_set$humidity) 

cor(bike_train_set.cor)
m<-cor(bike_train_set.cor)
corrplot(m, lower="square")
dim(bike_train_set.reduced)

#Lets implement PCA the attributes includes temp,atemp,humidity,windspeed
bike_train_set.required <- bike_train_set.reduced[,c(11:14)]
 head(bike_train_set.required)
dim(bike_train_set.required)
bike_train_set.manpca<-prcomp(bike_train_set.required,
                             center = TRUE,
                             scale. = TRUE)
print(bike_train_set.manpca)
# plot the PCA 
 par(mfrow=c(1,1))
plot(bike_train_set.manpca,type="l")
summary(bike_train_set.manpca)
# Lets predict
predict(bike_train_set.manpca)
install.packages("devtools")

install.packages("ggplot2")


library(devtools)
library(ggbiplot)
install_github("ggbiplot", "vqv")

g <- ggbiplot(bike_train_set.manpca, 
              obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

# PCA by Caret
library(caret)
bike_train_set.trans = preProcess(bike_train_set.required, 
                              method=c("BoxCox", "center", 
                                       "scale", "pca"))
bike_test_set.trans = preProcess(bike_train_set.required, 
                             method=c("BoxCox", "center", 
                                      "scale", "pca"))
bike_train_set.manpca = 
  predict(bike_train_set.trans, 
          bike_train_set.required)
# Predict the PCA
bike_test_set.manpca = 
  predict(bike_train_set.trans, 
          bike_train_set.required)
bike_test_set.new <- cbind(bike_test_set.reduced[,c(1,2,3,4,5)],bike_test_set.manpca)
bike_train_set.new <- cbind(bike_train_set.reduced[,c(1,2,3,4,5,6,7,8,9,10)],bike_train_set.manpca,bike_train_set.reduced[,c(15,16,17)])

#Lets plot PCA
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(devtools)
g <- ggbiplot(bike_train_set.manpca, 
              obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

