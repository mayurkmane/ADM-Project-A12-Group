#setwd("E:/kaggle mydata/bike sharing")

# We have read traindata and testdata mydata 
traindata=read.csv("train_bike.csv")
testdata=read.csv("test_bike.csv")
str(traindata)

#Initialised additional variables in testdata mydata
testdata$registered=0
testdata$casual=0
testdata$count=0

#Combined testdata and traindata mydata into mydata
mydata=rbind(traindata,testdata)
summary(mydata)

#foctorized season, weather, holiday  and workingday variables of mydata
mydata$season=as.factor(mydata$season)
mydata$weather=as.factor(mydata$weather)
mydata$holiday=as.factor(mydata$holiday)
mydata$workingday=as.factor(mydata$workingday)

#Made new variable hour in mydata
mydata$hour=substr(mydata$datetime,12,13)
mydata$hour=as.factor(mydata$hour)

#Seperated traindata and testdata mydata as per requirement i.e. within 19 days traindata mydata else considered as testdata mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]

#Visualised to check use of casual and registed users
boxplot(traindata$count~traindata$hour,xlab="hour", ylab="count of users")
boxplot(traindata$casual~traindata$hour,xlab="hour", ylab="casual users")
boxplot(traindata$registered~traindata$hour,xlab="hour", ylab="registered users")

#taken date as new variable and encorporated into mydata
date=substr(mydata$datetime,1,10)
days<-weekdays(as.Date(date))
mydata$day=days

#Seperated traindata and testdata mydata as per requirement i.e. within 19 days traindata mydata else considered as testdata mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]

#Visualised to check use of casual and registed users with different variables
#day
boxplot(traindata$registered~traindata$day,xlab="day", ylab="registered users")
boxplot(traindata$casual~traindata$day,xlab="day", ylab="casual users")

#weather
boxplot(traindata$registered~traindata$weather,xlab="weather", ylab="registered users")
boxplot(traindata$casual~traindata$weather,xlab="weather", ylab="casual users")

#temp
boxplot(traindata$registered~traindata$temp,xlab="temp", ylab="registered users")
boxplot(traindata$casual~traindata$temp,xlab="temp", ylab="casual users")

#created new variable year
mydata$year=substr(mydata$datetime,1,4)
mydata$year=as.factor(mydata$year)

#Seperated traindata and testdata mydata as per requirement i.e. within 19 days traindata mydata else considered as testdata mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]

#Visualised to check use of casual and registed users
#year
boxplot(traindata$registered~traindata$year,xlab="year", ylab="registered users")
boxplot(traindata$casual~traindata$year,xlab="year", ylab="casual users")

#windspeed
boxplot(traindata$registered~traindata$windspeed,xlab="year", ylab="registered users")
boxplot(traindata$casual~traindata$windspeed,xlab="year", ylab="casual users")

#humidity
boxplot(traindata$registered~traindata$humidity,xlab="humidity", ylab="registered users")
boxplot(traindata$casual~traindata$humidity,xlab="humidity", ylab="casual users")

mydata$hour=as.integer(mydata$hour)
mydata$day_part=0

#Seperated traindata and testdata mydata as per requirement i.e. within 19 days traindata mydata else considered as testdata mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]


#Combine testdata and traindata mydata
mydata=rbind(traindata,testdata)

#install.packages("rpart")
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#plot for registered vs hour
d=rpart(registered~hour,traindata)
fancyRpartPlot(d)

#plot for casual vs hour
d=rpart(casual~hour,traindata)
fancyRpartPlot(d)

#combine traindata and testdata mydata
mydata=rbind(traindata,testdata)

#divide register users based on hour variable
mydata$dp_reg=0
mydata$dp_reg[mydata$hour<8]=1
mydata$dp_reg[mydata$hour>=22]=2
mydata$dp_reg[mydata$hour>9 & mydata$hour<18]=3
mydata$dp_reg[mydata$hour==8]=4
mydata$dp_reg[mydata$hour==9]=5
mydata$dp_reg[mydata$hour==20 | mydata$hour==21]=6
mydata$dp_reg[mydata$hour==19 | mydata$hour==18]=7

#divide casual users based on hour variable
mydata$dp_cas=0
mydata$dp_cas[mydata$hour<=8]=1
mydata$dp_cas[mydata$hour==9]=2
mydata$dp_cas[mydata$hour>=10 & mydata$hour<=19]=3
mydata$dp_cas[mydata$hour>19]=4

#plot for registered users vs tempreture
f=rpart(registered~temp,traindata)
fancyRpartPlot(f)

#plot for casual users vs tempreture
f=rpart(casual~temp,traindata)
fancyRpartPlot(f)

#Divided registered users into 0 to 4 range based on tempreture varibles
mydata$temp_reg=0
mydata$temp_reg[mydata$temp<13]=1
mydata$temp_reg[mydata$temp>=13 & mydata$temp<23]=2
mydata$temp_reg[mydata$temp>=23 & mydata$temp<30]=3
mydata$temp_reg[mydata$temp>=30]=4

#Divided casual users into 0 to 4 range based on tempreture varibles
mydata$temp_cas=0
mydata$temp_cas[mydata$temp<15]=1
mydata$temp_cas[mydata$temp>=15 & mydata$temp<23]=2
mydata$temp_cas[mydata$temp>=23 & mydata$temp<30]=3
mydata$temp_cas[mydata$temp>=30]=4

#Categorised year into 8 parts
mydata$year_part[mydata$year=='2011']=1
mydata$year_part[mydata$year=='2011' & mydata$month>3]=2
mydata$year_part[mydata$year=='2011' & mydata$month>6]=3
mydata$year_part[mydata$year=='2011' & mydata$month>9]=4
mydata$year_part[mydata$year=='2012']=5
mydata$year_part[mydata$year=='2012' & mydata$month>3]=6
mydata$year_part[mydata$year=='2012' & mydata$month>6]=7
mydata$year_part[mydata$year=='2012' & mydata$month>9]=8
table(mydata$year_part)


#Categorised day into three different category in mydata
mydata$day_type=0
mydata$day_type[mydata$holiday==0 & mydata$workingday==0]="weekend"
mydata$day_type[mydata$holiday==1]="holiday"
mydata$day_type[mydata$holiday==0 & mydata$workingday==1]="working day"

#Divided testdata and traindata mydata from mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]

#plt tempreture vs count of users in traindata mydata
plot(traindata$temp,traindata$count)

#Combined traindata and testdata mydata
mydata=rbind(traindata,testdata)

#created month variable using datetime variable of mydata
mydata$month=substr(mydata$datetime,6,7)
mydata$month=as.integer(mydata$month)


table(mydata$windspeed==0)
k=mydata$windspeed==0
wind_0=subset(mydata,k)
wind_1=subset(mydata,!k)

#install.packages("randomForest")
library(randomForest)

#set seed use is to get same outcomes after each random iteration
set.seed(415)

#Applied random forest algorithm on our formula 
myfit <- randomForest(windspeed ~ season+weather +humidity +month+temp+ year+atemp, wind_1,importance=TRUE, ntree=250)
pred=predict(myfit,wind_0)
wind_0$windspeed=pred

#Updated mydata using new wind variable
mydata=rbind(wind_0,wind_1)
mydata$weekend=0
mydata$weekend[mydata$day=="Sunday" | mydata$day=="Saturday"]=1
str(mydata)

#Factorized variables in mydata
mydata$season=as.factor(mydata$season)
mydata$holiday=as.factor(mydata$holiday)
mydata$workingday=as.factor(mydata$workingday)
mydata$weather=as.factor(mydata$weather)
mydata$hour=as.factor(mydata$hour)
mydata$month=as.factor(mydata$month)
mydata$day_part=as.factor(mydata$dp_cas)
mydata$day_type=as.factor(mydata$dp_reg)
mydata$day=as.factor(mydata$day)
mydata$temp_cas=as.factor(mydata$temp_cas)
mydata$temp_reg=as.factor(mydata$temp_reg)

#Divided mydata into testdata and traindata mydata
traindata=mydata[as.integer(substr(mydata$datetime,9,10))<20,]
testdata=mydata[as.integer(substr(mydata$datetime,9,10))>19,]

#Updated registered and casual variables
traindata$reg1=traindata$registered+1
traindata$cas1=traindata$casual+1

#taken log of reg1 and cas1 
traindata$logcas=log(traindata$cas1)
traindata$logreg=log(traindata$reg1)
testdata$logreg=0
testdata$logcas=0

#plot for weather vs logarithmic vales of registered users
boxplot(traindata$logreg~traindata$weather,xlab="weather", ylab="registered users")

#plot for season vs logarithmic vales of registered users
boxplot(traindata$logreg~traindata$season,xlab="season", ylab="registered users")

set.seed(400)

#applied randomforeset algorithm on formula using required variables such as registered users of traindata and 200 trees
mymyfit1 <- randomForest(logreg ~ hour +workingday+day+holiday+ day_type +temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part, traindata,importance=TRUE, ntree=200)

#used model against testdata mydata
pred1=predict(mymyfit1,testdata)
testdata$logreg=pred1

set.seed(400)

#applied randomforeset algorithm on formula using required variables such as casual users of traindata and 200 trees
mymyfit2 <- randomForest(logcas ~hour + day_type+day+humidity+atemp+temp_cas+windspeed+season+weather+holiday+workingday+dp_cas+weekend+year+year_part, traindata,importance=TRUE, ntree=200)

#used model against testdata mydata
pred2=predict(mymyfit2,testdata)
testdata$logcas=pred2

#taken output from testdata mydata
testdata$registered=exp(testdata$logreg)-1
testdata$casual=exp(testdata$logcas)-1
testdata$count=testdata$casual+testdata$registered
s<-data.frame(datetime=testdata$datetime,count=testdata$count)
write.csv(s,file="randomforestOutput.csv",row.names=FALSE)

