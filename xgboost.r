xgboost_rte = function(cross.train, cross.test, train, test, plt = FALSE){
    GBM = caret::train(Load ~ Load.1 + Load.7 + toy
                       + Temp + Temp_s99_min + Temp_s99_max
                       + WeekDays,
                       data = train, method = "gbm", trace = F)
    summary(GBM)
    pred.gbm.train <- predict(GBM, newdata = train)
    pred.gbm.test <- predict(GBM, newdata = test)
    
    RMSE(pred.gbm.test[-N], to.test)
    return(list("pred.train" = pred.gbm.train, "pred.test" = pred.gbm.test))
}
