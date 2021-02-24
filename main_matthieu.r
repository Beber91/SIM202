rm(list=objects())
graphics.off()
library(tidyverse)
library(lubridate)
library(ranger)
library(pracma)
library(Metrics)
library(mgcv)
library(keras)
library(visreg)
library(caret)
library(mc2d)
library(opera)
library(abind)
library(randomForest)


setwd("C:/Users/CM/code/M1/R")

##load tous les fichiers en sources
files.sources = list.files()
sapply(files.sources, source) 

train <- read_delim(file="data/train_V2.csv",delim=',')
test <- read_delim(file="data/test_V2.csv",delim=',')

plot(train$Date, train$Load, type='l')
par(new=T)
plot(train$Date, train$Temp, type='l', col='red')

plot(train$Temp,train$Load, col='red')
reg = lm(Load~Temp, data=train)
points(train$Temp ,reg$fitted.values)

days_to_numeric = function (data) {
    data$WeekDays[data$WeekDays=='Monday']    = 1
    data$WeekDays[data$WeekDays=='Tuesday']   = 2
    data$WeekDays[data$WeekDays=='Wednesday'] = 3
    data$WeekDays[data$WeekDays=='Thursday']  = 4
    data$WeekDays[data$WeekDays=='Friday']    = 5
    data$WeekDays[data$WeekDays=='Saturday']  = 6
    data$WeekDays[data$WeekDays=='Sunday']    = 7
    data$WeekDays = as.numeric(data$WeekDays)
    return (data$WeekDays)
}

train$WeekDays = days_to_numeric(train)
test$WeekDays = days_to_numeric(test)

train$Year = train$Year - 2012
test$Year = test$Year - 2012

#saisonalite : annuelle

plot(train$Date, train$Load, type='l')

MA <- stats::filter(train$Load, filter = rep(1/365,365),
             method = c("convolution"), sides = 2, circular = FALSE)
plot(train$Date, train$Load, type = "l", xlab = "",
     ylab = "consumption (kw)", col = "seagreen4", lwd = 1)
lines(train$Date, MA, col = "red", lwd = 2)


##estimation avec fourier
fourier(train, test, plt = TRUE)

##saisonalite : hebdomadaire

num.years = 0
average = 0
N = 8; a = 0.7 #exponential weight
while (365*(num.years+1) <= length(train$Date)) {
    par(mfrow = c(1, 1))
    dateyear = train$Date[(365*num.years+1):(365*(num.years+1))]
    loadyear = train$Load[(365*num.years+1):(365*(num.years+1))]

    ##plot(dateyear,loadyear,type='l')
  
    MAw <- stats::filter(loadyear, filter = rep(1/52,52),
                      method = c("convolution"), sides = 2, circular = T)
    ##plot(dateyear,loadyear, type = "l", xlab = "",
    ##    ylab = "consumption (kw)", col = "seagreen4", lwd = 1)
    ##lines(dateyear, MAw, col = "red", lwd = 2)
  
    ##plot(dateyear, loadyear - MAw, type="l")
    expweight = (((1-a)/(1-a^8))*a^(N-(num.years+1)))
    average = expweight*(average + (loadyear - MAw))
    num.years = num.years + 1
}
plot(average)

train.to.day = train$time %% 365 + 1
test.to.day = test$time %% 365 + 1

pred.hebdo.train = average[train.to.day]
pred.hebdo.test = average[test.to.day]

pred.total.train = reg$fitted + pred.hebdo.train
pred.total.test = pred.fourier + pred.hebdo.test

par(mfrow=c(1,1))
plot(train$Load,type='l', xlim=c(0,length(total.time)))
lines(train$time,pred.total.train, col='red', lwd=1)
lines(test$time,pred.total.test, col='green', lwd=1)

Load = pred.total.test
Id = train$Id
submission = data.frame(Load, Id)

write.csv(submission, file ="submissions/submission.csv", row.names=F)


MAw.train.total <- stats::filter(train$Load.1, filter = rep(1/52,52),
                     method = c("convolution"), sides = 2, circular = T)
MAw.test.total <- stats::filter(test$Load.1, filter = rep(1/52,52),
                                 method = c("convolution"), sides = 2, circular = T)

train$seasonal.tendancy = train$Load.1 - MAw.train.total
test$seasonal.tendancy = test$Load.1 - MAw.test.total

train$fourier.fitted = reg$fitted
test$fourier.fitted = pred.fourier

reg2 = lm(Load~Load.1+Load.7+Temp
+Temp_s95+WeekDays+GovernmentResponseIndex,data=train)
summary(reg2)

Gam <- gam(Load~s(Load.1)+s(Load.7)+s(Temp)
           +s(Temp_s95)+s(WeekDays,k=7)
           +s(GovernmentResponseIndex)
           ,data=train)
summary(Gam)


gam.train = predict(Gam, newdata=train)
gam.test = predict(Gam, newdata=test)

par(mfrow=c(1,1))
plot(train$Load,type='l', xlim=c(0,length(total.time)))
lines(train$time,Gam$fit, col='red', lwd=1)
lines(test$time,gam.test, col='green', lwd=1)

##visreg(Gam,"Temp_s95")

evaluate = function(test_label, predicted_set){
  return(rmse(test_label, predicted_set))
}

tmp = format_data("./data/train_V2.csv", "./data/test_V2.csv")
test_label = tmp$test_label

Gam.rmse = evaluate(test_label, gam.test)

##cross-validation

cross.validation = function (train, test) {
  set.seed(123)
  to.extract = rbern(n=length(train$Load),p=0.8)==T
  to.train = train[F,]
  for (j in 1:length(to.extract)) {
    if (to.extract[j]) {
      to.train[nrow(to.train)+1,] = train[j,]
    }
  }
  
  cross.train  <- train[to.train$time, ]
  cross.test <- train[-to.train$time, ]
  
  model = gam(Load~s(Load.1)+s(Load.7)+s(Temp)
               +s(Temp_s95)+s(WeekDays,k=7)
               ,data=cross.train)
  
  pred.test = predict(model,test)
  print(length(pred.test))
  N = length(test$Load.1)
  RMSE = rmse(pred.test[-N], test$Load.1[2:N]) #on connait pas le dernier
  
  print(RMSE)
  return(list("model"=model,"pred.test"=pred.test,"RMSE"=RMSE))
}

cv = cross.validation(train, test)
model = cv$model; pred.test = cv$pred.test; RMSE = cv$RMSE


par(mfrow=c(1,1))
plot(train$Load,type='l', xlim=c(0,length(total.time)))
lines(train$time,predict(model,train), col='red', lwd=1)
lines(test$time,pred.test, col='green', lwd=1)

Load = pred.test
Id = 1:length(Load)
submission = data.frame(Load, Id)

write.csv(submission, file ="submission.csv", row.names=F)

##lstm

labels = train$Load
train.lstm = train[,-c(1,2,22,23)]
test.lstm = test[,-c(1,20,21,23,24)]

lstm = function(train_set, train_label, test_set) {
  y = train_label
  x = data.matrix(train_set)
  x_test = data.matrix(test_set)
  model = keras_model_sequential() %>%
    layer_dense(units = 19, activation = 'relu', input_shape = c(19)) %>%
    layer_dense(units = 12, activation = 'relu') %>%
    layer_dense(units = 6, activation = 'relu') %>%
    layer_dense(units=1, activation = "linear")
  
  model %>% compile(loss = 'mse',
                    optimizer = optimizer_adam(0.0005))
  
  model %>% fit(x,y, epochs=15)

  pred.train = model %>% predict(x)
  pred.test = model %>% predict(x_test)

  return(list("train"=pred.train,"test"=pred.test))
}


pred.lstm = lstm(train.lstm, labels, test.lstm)

par(mfrow=c(1,1))
plot(train$Load,type='l', xlim=c(0,length(total.time)))
lines(test$time,pred.lstm$test, col='green', lwd=1)

N = length(test$Load.1)
RMSE = rmse(pred.lstm$test[-N], test$Load.1[2:N])
RMSE


##random forest

rf = randomForest(Load ~ ., data=train, mtry=3,
                         importance=TRUE, na.action=na.omit)
pred.test.rf = predict(rf,test)

par(mfrow=c(1,1))
plot(train$Load,type='l', xlim=c(0,length(total.time)))
lines(test$time,pred.test.rf, col='green', lwd=1)

N = length(test$Load.1)
RMSE = rmse(pred.test.rf[-N], test$Load.1[2:N])
RMSE


##aggregation

expert1.train  = predict(model,train)
expert2.train  = pred.lstm$train
expert3.train  = rf$predicted

expert1.test  = pred.test
expert2.test = pred.lstm$test
expert3.test  = pred.test.rf

experts.train = array_reshape(abind(expert1.train,expert2.train,expert3.train,
                              along=2), c(3028,1,3))

experts.test = cbind(expert1.test,expert2.test,expert3.test)

oracle1 = mixture(
            Y = train$Load,
            experts = experts.train,
            model = "EWA",
            loss.type = "square",
            coefficients = "Uniform",
)
coeffs1 = oracle1$coefficients
print(oracle1)

##alternative

oracle2 <- oracle(Y = train$Load,
                        experts = experts.train,
                        loss.type = "square", model = "convex"
)
plot(oracle2)
coeffs2 = oracle2$coefficients
coeffs2 = as.vector(coeffs2)
print(coeffs2)

pred.oracle = experts.test %*% coeffs1;

par(mfrow=c(1,1))
plot(train$Load,type='l', xlim=c(0,length(total.time)))
lines(test$time,pred.oracle, col='green', lwd=1)

N = length(test$Load.1)
RMSE = rmse(pred.oracle[-N], test$Load.1[2:N])
RMSE

Load = pred.oracle
Id = 1:length(test$Load.1)
submission = data.frame(Load, Id)

write.csv(submission, file ="submission.csv", row.names=F)

