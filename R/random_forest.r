#' @export
random_forest = function(cross.train, cross.test, train, test, n_trees, plt = FALSE) {
    print("Random Forest")
    rf = randomForest::randomForest(Load ~ ., data=cross.train, mtry=n_trees,
                      importance=TRUE, na.action=na.omit)
    pred.rf.rmse = caret::RMSE(predict(rf, cross.test), cross.test$Load)
    rf = randomForest::randomForest(Load ~ ., data=train, mtry=n_trees,
                      importance=TRUE, na.action=na.omit)
    pred.rf.train = predict(rf,train)
    pred.rf.test = predict(rf,test)
    if (plt) {
        plot(train$Load, type='l', xlim=c(0,length(total.time)),
             main="Prédiction de la Random Forest", xlab="Temps", ylab="Load",
             sub=paste("RMSE =",pred.rf.rmse))
        lines(train$time, pred.rf.train, col='red', lwd=1)
        lines(test$time, pred.rf.test, col='green', lwd=1)
    }
    return(list("pred.train" = pred.rf.train, "pred.test" = pred.rf.test, "rmse" = pred.rf.rmse))
 }
