#' @export
xgboost_rte = function(cross.train, cross.test, train, test, plt = FALSE){
    print("XGBoost")
    tr.Control = caret::trainControl(method = "cv", number = 10, allowParallel = T,
                              verboseIter = F, returnData = F)
    XGB.Grid = expand.grid(nrounds = c(100,200),
                           max_depth = c(3, 5, 10, 15, 20),
                           colsample_bytree = seq(0.5, 0.9, length.out = 5),
                           eta = 0.1,
                           gamma=0,
                           min_child_weight = 1,
                           subsample = 1
                           )
    XGB = xgboost::xgboost(data = data.matrix(dplyr::select(cross.train,-"Load")),
                  label = data.matrix(dplyr::select(cross.train,"Load")),
                  nrounds = 15,
                  trControl = tr.Control, tuneGrid = XGB.Grid,
                  objective = "reg:squarederror", verbose = 2)
    #summary(XGB)
    pred.xgb.train = predict(XGB, newdata = data.matrix(dplyr::select(train,-"Load")))
    pred.xgb.test = predict(XGB, newdata = data.matrix(test))

    pred.xgb.rmse = caret::RMSE(predict(XGB, newdata = data.matrix(dplyr::select(cross.test,-"Load"))), cross.test$Load)
    if (plt){
        plot(train$Load, type='l', xlim=c(0,length(total.time)),
             main="Prédiction d'XGBoost", xlab="Temps", ylab="Load",
             sub=paste("RMSE =",pred.xgb.rmse))
        lines(train$time, pred.xgb.train, col='red', lwd=1)
        lines(test$time, pred.xgb.test, col='green', lwd=1)
    }
    return(list("pred.train" = pred.xgb.train, "pred.test" = pred.xgb.test, "rmse" = pred.xgb.rmse))
}
