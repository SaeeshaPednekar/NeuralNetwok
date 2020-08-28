concrete <- read.csv("F:/Excelr/Assignments/dataset/neural network/concrete.csv")
View(concrete)
str(concrete)
attach(concrete)
sum(is.na(concrete))
#exploratory data analysis

mean(age)
mean(ash)
mean(cement)
mean(coarseagg)
mean(fineagg)
mean(slag)
mean(strength)
mean(superplastic)
mean(water)

median(age)
median(ash)
median(cement)
median(coarseagg)
median(fineagg)
median(slag)
median(strength)
median(superplastic)
median(water)

sd(age)
sd(ash)
sd(cement)
sd(coarseagg)
sd(fineagg)
sd(slag)
sd(strength)
sd(superplastic)
sd(water)


var(age)
var(ash)
var(cement)
var(coarseagg)
var(fineagg)
var(slag)
var(strength)
var(superplastic)
var(water)


library(moments)
skewness(age)
skewness(ash)
skewness(cement)
skewness(coarseagg)
skewness(fineagg)
skewness(slag)
skewness(strength)
skewness(superplastic)
skewness(water)

kurtosis(age)
kurtosis(ash)
kurtosis(cement)
kurtosis(coarseagg)
kurtosis(fineagg)
kurtosis(slag)
kurtosis(strength)
kurtosis(superplastic)
kurtosis(water)

?hist?lines
hist(age,probability = T,breaks = 30)
lines(density(age))
hist(ash)
hist(cement,probability = T,breaks = 30)
lines(density(cement))
hist(coarseagg)
hist(fineagg,probability = T,breaks = 30)
lines(density(fineagg))
hist(slag)
hist(strength,probability = T,breaks = 30)
lines(density(strength))
hist(superplastic)
hist(water)



boxplot(age,horizontal = T)
boxplot(ash,horizontal = T)
boxplot(cement,horizontal = T)
boxplot(coarseagg,horizontal = T)
boxplot(fineagg,horizontal = T)
boxplot(slag,horizontal = T)
boxplot(strength,horizontal = T)
boxplot(superplastic,horizontal = T)
boxplot(water,horizontal = T)


barplot(age)
barplot(ash)
barplot(cement)
barplot(coarseagg)
barplot(fineagg)
barplot(slag)
barplot(strength)
barplot(superplastic)
barplot(water)

plot(age,strength)
cor(age,strength)
plot(ash,strength)
cor(ash,strength)
plot(cement,strength)
cor(cement,strength)
plot(coarseagg,strength)
cor(coarseagg,strength)
plot(fineagg,strength)
cor(fineagg,strength)
plot(slag,strength)
cor(slag,strength)
plot(superplastic,strength)
cor(superplastic,strength)
plot(water,strength)
cor(water,strength)

cor(concrete)
pairs(concrete)

library(corpcor)
cor(concrete)
cor2pcor(cor(concrete))

#normalise the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN = normalize))
View(concrete_norm)

#splitting data into train and test
install.packages("caTools")
library(caTools)
split_data<-sample.split(concrete$strength,SplitRatio = 0.70)
split_data

concrete_train<-subset(concrete_norm,split_data==TRUE)
concrete_test<-subset(concrete_norm,split_data==FALSE)
library(ggplot2)

#ANN
library(neuralnet)
concrete_model<-neuralnet(strength~.,data=concrete_train)
plot(concrete_model)

#training model
train<-compute(concrete_model,concrete_train[1:8])
cor(train$net.result,concrete_train$strength)
#testing
test<-compute(concrete_model,concrete_test[1:8])
cor(test$net.result,concrete_test$strength)
plot(test$net.result,concrete_test$strength)


#model new
model_new<-neuralnet(strength~.,data=concrete_train,hidden=c(2,2))
plot(model_new)
#training on new model
train_new<-compute(model_new,concrete_train[-9])
cor(train_new$net.result,concrete_train$strength)
plot(train_new$net.result,concrete_train$strength)

#testing on new model
test_new<-compute(model_new,concrete_test[-9])

plot(test_new$net.result,concrete_test$strength)
cor(test_new$net.result,concrete_test$strength)



#model_new_1
model_new_1<-neuralnet(strength~.,data=concrete_train,hidden=c(5,5))
plot(model_new_1)
#training on new model
train_new_1<-compute(model_new_1,concrete_train[-9])
cor(train_new_1$net.result,concrete_train$strength)
plot(train_new_1$net.result,concrete_train$strength)

#testing on new model
test_new_1<-compute(model_new_1,concrete_test[-9])

plot(test_new_1$net.result,concrete_test$strength)
cor(test_new_1$net.result,concrete_test$strength)


