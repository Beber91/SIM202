rm(list=objects())
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(Metrics))
source("format_data.R")
source("evaluate.R")

data = format_data("./data/train_V2.csv", "./data/test_V2.csv")
train_set = data$train_set
test_set = data$test_set
train_label = data$train_label
test_label = data$test_label

param = list(booster = "gblinear", objective = "reg:squarederror", eval_metric = "rmse", lambda = 0.0003, alpha = 0.0003, nthread = 2, eta = 0.1)

print("Model : XGBOOST")

xgbmodel = xgboost(data = train_set, label = train_label, nrounds = 200, params = param, verbose = 0)

pred = predict(xgbmodel, test_set)

print(paste("Score final : ", evaluate(test_label, pred), sep=""))
