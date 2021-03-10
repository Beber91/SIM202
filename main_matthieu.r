## A FAIRE SVM + ACF ET PCF SUR LOAD + SITE FAVORI + STACKING

rm(list=objects())
graphics.off()

##pour load tout en 2 lignes, il faut juste rajouter les librairies dans libs.to.load

libs.to.load = c("tidyverse", "lubridate", "ranger",
                 "pracma", "Metrics", "mgcv", "keras",
                 "visreg", "caret", "mc2d", "opera", "abind",
                 "randomForest", "tensorflow","plot3D","e1071"
                 ,"numbers","glmnet","neuralnet")
suppressPackageStartupMessages(sapply(libs.to.load, require, character.only = TRUE))

##setwd("C:/Users/CM/code/M1/R")

##load tous les fichiers en sources
files.sources = list.files(pattern = "*.r$")
files.sources = files.sources[files.sources != "main_matthieu.r"]
sapply(files.sources, source)

##
plt = F #(si on veut plot, mettre à TRUE)
path_submission = "./submissions/submission.csv"


##MAIN NICOLAS
model = "aucun"

data = format_data("./data/train_V2.csv", "./data/test_V2.csv")
train_set = data$train_set
test_set = data$test_set
train_label = data$train_label
test_label = data$test_label

if (model == "xgboost"){
  pred = xgboost_rte(train_set, train_label, test_set)
} else if(model == "lstm"){
  pred = lstm(train_set, train_label, test_set)
}

print(paste("Score final : ", evaluate(test_label, pred), sep=""))

if (plt) {
    plot(c(train_label, pred))
}

##FIN MAIN NICOLAS 


train <- read_delim(file="data/train_V2.csv",delim=',')
test <- read_delim(file="data/test_V2.csv",delim=',')

reg = lm(Load~Temp, data=train)

if (plt){
    plot(train$Date, train$Load, type='l')
    par(new=T)
    plot(train$Date, train$Temp, type='l', col='red')

    plot(train$Temp,train$Load, col='red')
    points(train$Temp ,reg$fitted.values)
}

train$WeekDays = days_to_numeric(train)
test$WeekDays = days_to_numeric(test)

train$Year = train$Year - 2012
test$Year = test$Year - 2012

total.time = c(1:(nrow(train)+nrow(test)))
length(total.time)
train$time = total.time[1:nrow(train)]
test$time = tail(total.time,nrow(test))


#saisonalite : annuelle

if (plt){
    plot(train$Date, train$Load, type='l')
}

MA <- stats::filter(train$Load, filter = rep(1/365,365),
             method = c("convolution"), sides = 2, circular = FALSE)
if (plt){
    plot(train$Date, train$Load, type = "l", xlab = "",
         ylab = "consumption (kw)", col = "seagreen4", lwd = 1)
    lines(train$Date, MA, col = "red", lwd = 2)
}

##estimation avec fourier
pred.fourier = fourier(train, test, plt = TRUE)


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
if (plt){
    plot(average)
}

train.to.day = train$time %% 365 + 1
test.to.day = test$time %% 365 + 1

pred.hebdo.train = average[train.to.day]
pred.hebdo.test = average[test.to.day]

pred.total.train = reg$fitted + pred.hebdo.train
pred.total.test = pred.fourier + pred.hebdo.test

if (plt){
    par(mfrow=c(1,1))
    plot(train$Load,type='l', xlim=c(0,length(total.time)))
    lines(train$time,pred.total.train, col='red', lwd=1)
    lines(test$time,pred.total.test, col='green', lwd=1)
}

Load = pred.total.test
Id = test$Id
submission = data.frame(Load, Id)

write.csv(submission, file =path_submission, row.names=F)


MAw.train.total <- stats::filter(train$Load.1, filter = rep(1/52,52),
                     method = c("convolution"), sides = 2, circular = T)
MAw.test.total <- stats::filter(test$Load.1, filter = rep(1/52,52),
                                 method = c("convolution"), sides = 2, circular = T)

train$seasonal.tendancy = train$Load.1 - MAw.train.total
test$seasonal.tendancy = test$Load.1 - MAw.test.total

train$fourier.fitted = reg$fitted
test$fourier.fitted = pred.fourier

## scaling data
train = select(train,-c('Date','seasonal.tendancy','fourier.fitted'))
test = select(test,-c('Date','seasonal.tendancy','fourier.fitted','Usage','Id'))

Load = train$Load; train.time = train$time; test.time = test$time

train.scaled = as.data.frame(scale(train, center = T, scale = T))
train.scaled$Load = Load; train$time = train.time

test.scaled = as.data.frame(scale(test, center = T, scale = T))
test.scaled$time = test.time

N = length(test$Load.1)
to.test = test$Load.1[2:N]

## cross-validation

cv = cross_validation(train)
cross.train = cv$train; cross.test = cv$test

## GAM

model = gam(Load~s(Load.1,k=6)+s(Load.7,k=7)+s(Temp,k=6,bs="cr")
            +s(WeekDays,k=7)+s(toy,k=6,bs="cr")
            +ti(toy,Temp,bs="cr")+ti(toy,Load.1,bs="cr")
            +ti(toy,Temp_s99_min,k=6,bs="cr")
            +ti(WeekDays,Load.1,k=6)
            ,data=cross.train)
summary(model)

pred.gam.train = predict(model,select(train,-'Load'))
pred.gam.test = predict(model,test)

N = length(test$Load.1)
RMSE = rmse(pred.gam.test[-N], to.test) #on connait pas le dernier
print(RMSE)

##scatter3D(train$Load.1,train$Load.7,train$Load, theta = 50, phi = 20)

if (plt){
    par(mfrow=c(1,1))
    plot(train$Load,type='l', xlim=c(0,length(total.time)))
    lines(train$time,predict(model,train), col='red', lwd=1)
    lines(test$time,pred.test, col='green', lwd=1)
}

## LSTM

NN = neural_network(cross.train, test)
NN$test

plot(NN$test,type='l')

rmse(NN$test[-275], to.test)

## random forest

tuneRF(select(train, -'Load'),train$Load)
n_trees = 12
res = random_forest(cross.train, test, n_trees)
pred.test.rf = res$pred.test.rf; rf = res$rf

##rf$importance
##varImpPlot(rf)

pred.train.rf = predict(rf,train)

rmse(pred.test.rf[-N], to.test)

## SVM

aaa = tune(svm, Load ~ toy + Temp + Temp_s99_min +
        Load.1 + Load.7 + WeekDays, data=cross.train,
        ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

SVM = aaa$best.model

pred.svm.test = predict(SVM, test)
pred.svm.train = predict(SVM, train)

rmse(pred.svm.train, train$Load)
rmse(pred.svm.test[-N], to.test)

if (plt){
  par(mfrow=c(1,1))
  plot(train$Load,type='l', xlim=c(0,length(total.time)))
  lines(train$time,pred.svm.train, col='red', lwd=1)
  lines(test$time,pred.svm.test, col='green', lwd=1)
}

## Elastic-Net

elastic = cv.glmnet(data.matrix(select(cross.train,-'Load')),cross.train$Load,
          family="gaussian",type.measure="mse",alpha=0.7)

lambda.min = elastic$lambda.min

pred.elastic.train = predict(elastic,data.matrix(select(train,-'Load'))
                             ,s=lambda.min)
pred.elastic.test = predict(elastic,data.matrix(test),s=lambda.min)

rmse(pred.elastic.test[-N], to.test)

## NN


##aggregation

list_experts_train = abind(pred.gam.train, 
                           #pred.lstm,
                           pred.train.rf,
                           pred.svm.train,
                           pred.elastic.train,
                           along = 2)
experts.test = cbind(pred.gam.test, #NN$test, 
                     pred.test.rf, pred.svm.test,pred.elastic.test)


add_expert = function(list_experts_train){
    experts.train = array_reshape(abind(list_experts_train), c(3028,1,length(list_experts_train)/3028))
    return(experts.train)
}

experts.train = add_expert(list_experts_train)


oracle1 = mixture(
            Y = train$Load,
            experts = experts.train,
            model = "OGD",
            loss.type = "square",
            coefficients = "Uniform",
)
coeffs1 = oracle1$coefficients
print(oracle1)

pred.oracle = experts.test %*% coeffs1#c(0.4,0.4,0.1,0.1)##coeffs2

par(mfrow=c(1,1))
plot(train$Load,type='l', xlim=c(0,length(total.time)))
lines(test$time,pred.oracle, col='green', lwd=1)

N = length(test$Load.1)
RMSE = rmse(pred.oracle[-N], test$Load.1[2:N])
RMSE

Load = pred.oracle
Id = 1:length(test$Load.1)
submission = data.frame(Load, Id)

write.csv(submission, file =path_submission, row.names=F)


