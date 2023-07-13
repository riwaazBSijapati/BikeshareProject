setwd("C:/Users/Goshawk/Desktop/Classes/FALL 2019/STA/Project/bike-sharing-dataset")
day2=read.csv("day2.csv")
dim(day2)
summary(day2)
library(MASS)
library(boot)
install.packages(DAAG)
attach(day2)
install.packages(bootstrap)
library("bootstrap")
#model creation and stepwise regression
day.lm=lm(count~season+year+month+holiday+weekday+workingday+weathertype+acttempdenorm+tempdenorm+humiditydenorm+windspeeddenorm)
summary(day.lm)
step=stepAIC(day.lm,direction="both")
step$anova
day.lm2=lm(count~season+year+month+holiday+weekday+weathertype+tempdenorm+humiditydenorm+windspeeddenorm)
summary(day.lm2)
#low mse or high acc rate for model: basic cv
set.seed(10)
train = sample(730, 365)
day.lmtrain=lm(count~season+year+month+holiday+weekday+weathertype+tempdenorm+humiditydenorm+windspeeddenorm,subset=train)
mean((count-predict(day.lmtrain,day2))[-train^2])
#loocv
glm.fit=glm(count~season+year+month+holiday+weekday+weathertype+tempdenorm+humiditydenorm+windspeeddenorm)
summary(glm.fit)
coef(glm.fit)
cv.err=cv.glm(day2,glm.fit)
cv.err$delta
#k-fold
set.seed(10)
daydataframe=data.frame(train)
cv.lm(df=daydataframe,glm.fit,m=10)
#cv validation set app
#https://feliperego.github.io/blog/2015/03/23/Resampling-Validation-Set-Approach-Example
# followed as shown above
set.seed(10)
tday=sample(731,.5*731)
day.lmtrain=lm(count~season+year+month+holiday+weekday+weathertype+tempdenorm+humiditydenorm+windspeeddenorm,subset=tday)
par(mfrow=c(2,2))
plot(day.lmtrain,pch=16)
sqrt(mean((day2$count-predict(day.lmtrain,day2))[-tday]^2))
rmse=c()
for(i in 1:100){
  train=sample(731,.5*731)
  tst=setdiff(731,.5*731)
    day.lmtrain2=lm(count~season+year+month+holiday+weekday+weathertype+tempdenorm+humiditydenorm+windspeeddenorm,data=day2[train,])
    preds=predict(day.lmtrain2,tester=day2[tst,])
    rmse[i]=sqrt(mean((day2$count-predict(day.lmtrain2,day2))[tst]^2))
}
mean(rmse)
hist(rmse,density=35,main="Test RMSE over 100 samples", xlab="Value of Obtained RMSE",col="Blue",border="Black")
par(mfrow=c(1,1))
abline(v=mean(rmse),lwd=3,col="red")
#valid set
#count data
library("DAAG")
