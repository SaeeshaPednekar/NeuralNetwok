startups<-read.csv("F:/Excelr/Assignments/dataset/neural network/50_startups.csv")
View(startups)
summary(startups)
sum(is.na(startups))
str(startups)
class(startups)
attach(startups)
library(plyr)
startups$State <- as.numeric(revalue(startups$State, c("New York"="0", "California"="1", "Florida"="2"))) 
str(startups)



#exploratory data analysis
mean(R.D.Spend)
mean(Administration)
mean(Marketing.Spend)
mean(Profit)


median(R.D.Spend)
median(Administration)
median(Marketing.Spend)
median(Profit)

var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(Profit)

sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)

library(moments)
hist(R.D.Spend,probability = T,breaks = 30)
lines(density(R.D.Spend))
hist(Administration,probability = T,breaks = 30)
lines(density(Administration))
hist(Marketing.Spend,probability = T,breaks = 30)
lines(density(Marketing.Spend))
hist(Profit,probability = T,breaks = 30)
lines(density(Profit))
hist(State,probability = T,breaks = 30)
lines(density(State))


skewness(R.D.Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(Profit)

boxplot(R.D.Spend,horizontal = T)
boxplot(Administration,horizontal = T)
boxplot(Marketing.Spend,horizontal = T)
boxplot(Profit,horizontal = T)
boxplot(State,horizontal = T)

kurtosis(R.D.Spend)
kurtosis(Administration)
kurtosis(Marketing.Spend)
kurtosis(Profit)


barplot(R.D.Spend)
barplot(Administration)
barplot(Marketing.Spend)
barplot(Profit)

plot(R.D.Spend,Profit)
cor(R.D.Spend,Profit)
plot(Administration,Profit)
cor(Administration,Profit)
plot(Marketing.Spend,Profit)
cor(Marketing.Spend,Profit)
plot(State,Profit)
cor(State,Profit)




normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

startups_norm<-as.data.frame(lapply(startups,FUN = normalize))
View(startups_norm)

#splitting the data
set.seed(123)
data<-sample(1:nrow(startups_norm),size=nrow(startups_norm)*0.7,replace = FALSE)

startups_train<-startups_norm[data,]
startups_test<-startups_norm[-data,]


#building a model
library(neuralnet)
startups_model<-neuralnet(Profit~.,data = startups_train)
plot(startups_model)

#tarining model
train_model<-compute(startups_model,startups_train[-5])
plot(train_model$net.result,startups_train$Profit)
cor(train_model$net.result,startups_train$Profit)

test_model<-compute(startups_model,startups_test[-5])
plot(test_model$net.result,startups_test$profit)
cor(test_model$net.result,startups_test$profit)

