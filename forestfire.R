forestfire<-read.csv("F:/Excelr/Assignments/dataset/neural network/forestfires.csv")
View(forestfire)
summary(forestfire)
str(forestfire)
library(plyr)
forestfire$month<-as.numeric(revalue(forestfire$month,c("jan"="1","feb"="2","mar"="3","apr"="4","may"="5","jun"="6","jul"="7","aug"="8","sep"="9","oct"="10","nov"="11","dec"="12")))
forestfire$day<-as.numeric(revalue(forestfire$day,c("mon"="1","tue"="2","wed"="3","thu"="4","fri"="5","sat"="6","sun"="7")))
forestfire$size_category<-as.numeric(revalue(forestfire$size_category,c("large"="1","small"="2")))

attach(forestfire)
str(forestfire)

#exploratory data analysis

boxplot(FFMC,horizontal = T)
boxplot(DMC,horizontal = T)
boxplot(DC,horizontal = T)
boxplot(ISI,horizontal = T)
boxplot(temp,horizontal = T)
boxplot(RH,horizontal = T)
boxplot(wind,horizontal = T)
boxplot(area,horizontal = T)
library(moments)

skewness(FFMC)
skewness(DMC)
skewness(DC)
skewness(ISI)
skewness(temp)
skewness(RH)
skewness(wind)
skewness(area)

kurtosis(FFMC)
kurtosis(DMC)
kurtosis(DC)
kurtosis(ISI)
kurtosis(temp)
kurtosis(RH)
kurtosis(wind)
kurtosis(area)

plot(FFMC,area)
cor(FFMC,area)
plot(DMC,area)
cor(DMC,area)
plot(DC,area)
cor(DC,area)
plot(ISI,area)
cor(ISI,area)
plot(temp,area)
cor(temp,area)
plot(RH,area)
cor(RH,area)
plot(wind,area)
cor(ISI,area)
plot(wind,area)
cor(wind,area)

hist(area,probability = T,breaks = 30)
lines(density(area))
hist(FFMC,probability = T,breaks = 30)
lines(density(FFMC))
hist(DMC,probability = T,breaks = 30)
lines(density(DMC))
hist(DC,probability = T,breaks = 30)
lines(density(DC))
hist(ISI,probability = T,breaks = 30)
lines(density(ISI))
hist(temp,probability = T,breaks = 30)
lines(density(temp))
hist(RH,probability = T,breaks = 30)
lines(density(RH))
hist(wind,probability = T,breaks = 30)
lines(density(wind))


normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

forest_norm<-as.data.frame(lapply(forestfire,FUN=normalize))

#splitting the data
set.seed(123)
data<-sample(1:nrow(forest_norm),nrow(forest_norm)*0.7,replace = FALSE)

forestfire_train<-forest_norm[data,]
forestfire_test<-forest_norm[-data,]

#building ann model
library(neuralnet)
forest_model<-neuralnet(area~.,data=forestfire_train)
plot(forest_model)


train_model<-compute(forest_model,forestfire_train[-11])
cor(train_model$net.result,forestfire_train$area)
plot(train_model$net.result,forestfire_train$area)

test_model<-compute(forest_model,forestfire_test[-11])
plot(test_model$net.result,forestfire_test$area)
cor(test_model$net.result,forestfire_test$area)


#model_new
model_new<-neuralnet(area~.,data=forestfire_train,hidden = c(5,5))

train_new_model<-compute(model_new,forestfire_train[-11])
cor(train_new_model$net.result,forestfire_train$area)

test_new_model<-compute(model_new,forestfire_test[-11])
cor(test_new_model$net.result,forestfire_test$area)
