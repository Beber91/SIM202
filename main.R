rm(list=objects())
graphics.off()
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ranger))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(Metrics))
library(keras)
library(tensorflow)
source("format_data.R")
source("evaluate.R")
source("LSTM.R")
source("xgboost.R")
source("scale.R")

model = "lstm"

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

plot(c(train_label, pred))