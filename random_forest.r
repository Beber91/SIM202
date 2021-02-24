random_forest = function(train, test, plt = FALSE){

    rf = randomForest(Load ~ ., data=train, mtry=3,
                      importance=TRUE, na.action=na.omit)
    pred.test.rf = predict(rf,test)

    if (plt){
        par(mfrow=c(1,1))
        plot(train$Load,type='l', xlim=c(0,length(total.time)))
        lines(test$time,pred.test.rf, col='green', lwd=1)
    }

    N = length(test$Load.1)
    RMSE = rmse(pred.test.rf[-N], test$Load.1[2:N])
    RMSE
    return(list("pred.test.rf" = pred.test.rf, "rf" = rf))
 }
