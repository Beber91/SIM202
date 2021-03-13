## A FAIRE SVM + ACF ET PCF SUR LOAD + SITE FAVORI + STACKING

rm(list=objects())
graphics.off()

##pour load tout en 2 lignes, il faut juste rajouter les librairies dans libs.to.load

libs.to.load = c("tidyverse", "lubridate", "ranger",
                 "pracma", "Metrics", "mgcv", "keras",
                 "visreg", "caret", "mc2d", "opera", "abind",
                 "randomForest", "tensorflow","plot3D","e1071"
                 ,"numbers","glmnet","neuralnet", "forecast", "xts")
suppressPackageStartupMessages(sapply(libs.to.load, require, character.only = TRUE))

##setwd("C:/Users/CM/code/M1/R")

##load tous les fichiers en sources
files.sources = list.files(pattern = "*.r$")
files.sources = files.sources[files.sources != "main_matthieu.r"]
files.sources = files.sources[files.sources != "main.r"]
sapply(files.sources, source)

##Environment var
plt = F #(si on veut plot, mettre à TRUE)
path_submission = "./submissions/submission.csv"

## Format data
model = "aucun"
date1 = strptime("01/01/2012", "%d/%m/%Y")
date2 = strptime("16/04/2020", "%d/%m/%Y")
Date = seq(date1, date2, by = "1 day")

data = format_data("./data/train_V2.csv", "./data/test_V2.csv")
train_set = data$train_set
test_set = data$test_set
train_label = data$train_label
test_label = data$test_label

data_mat = format_data_mat("./data/train_V2.csv", "./data/test_V2.csv")
train = data_mat$train; test = data_mat$test

## Fourier

pred.fourier = fourier(train, test, plt = plt)

## scale data

data_sc = scdat(train, test)
train = data_sc$train; test = data_sc$test

## cross-validation

cv = cross_validation(train)
cross.train = cv$train; cross.test = cv$test

## GAM

pred.gam = gam_rte(cross.train, cross.test, train, test)
pred.gam.train = pred.gam$pred.train; pred.gam.test = pred.gam$pred.test

## Random forest

tuneRF(select(train, -'Load'),train$Load)
n_trees = 12
pred.rf = random_forest(cross.train, test, n_trees)
pred.rf.train = pred.rf$pred.train ; pred.rf.test = pred.rf$pred.test; rf = pred.rf$rf

## SVM

pred.svm = SVM(cross.train, cross.test, train, test)
pred.svm.train = pred.svm$pred.train; pred.svm.test = pred.svm$pred.test

## Elastic-Net

pred.elastic = elastic(cross.train, cross.test, train, test)
pred.elastic.train = pred.elastic$pred.train; pred.elastic.test = pred.elastic$pred.test

## Ridge

pred.ridge = ridge(cross.train, cross.test, train, test)
pred.ridge.train = pred.ridge$pred.train; pred.ridge.test = pred.ridge$pred.test

## Lasso

pred.lasso = ridge(cross.train, cross.test, train, test)
pred.lasso.train = pred.lasso$pred.train; pred.lasso.test = pred.lasso$pred.test

## ARIMA

pred.arima = arima_rte(train_label, test_label, Date)
pred.arima.train = pred.arima$pred.train; pred.arima.test = pred.arima$pred.test

## Linear Model

pred.lm = linmod(cross.train, cross.test, train, test)
pred.lm.train = pred.lm$pred.train; pred.lm.test = pred.lm$pred.test

## Gradient Boosting

pred.gbm = xgboost_rte(cross.train, cross.test, train, test)
pred.gbm.train = pred.gbm$pred.train; pred.gbm.test = pred.gbm$pred.test

## Expert Agregation


list_experts_train = abind(pred.gam.train, 
                           pred.rf.train,
                           pred.svm.train, 
                           pred.elastic.train,
                           pred.ridge.train,
                           pred.lasso.train,
                           pred.arima.train,
                           pred.lm.train,
#                           pred.gbm.train,
                           along = 2)

experts.test = cbind(pred.gam.test, 
                     pred.rf.test,
                     pred.svm.test,
                     pred.elastic.test,
                     pred.ridge.test,
                     pred.lasso.test,
                     pred.arima.test,
                     pred.lm.test,
#                     pred.gbm.test)
experts.train = add_expert(list_experts_train)

oracle1 = mixture(
            Y = train_label,
            experts = experts.train,
            model = "OGD",
            loss.type = "square",
            coefficients = "Uniform",
)
coeffs1 = oracle1$coefficients
print(oracle1)

pred.oracle = experts.test %*% coeffs1#c(0.4,0.4,0.1,0.1)##coeffs2
if(plt){
    par(mfrow=c(1,1))
    plot(train_label,type='l', xlim=c(0,length(total.time)))
    lines(test$time,pred.oracle, col='green', lwd=1)
}
N = length(test$Load.1)
RMSE = rmse(pred.oracle[-N], to.test)
RMSE


