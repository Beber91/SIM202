#' @export
linmod = function(cross.train, cross.test, train, test, plt = FALSE){
    print("Modèle Linéaire")
    modelLM = lm(Load~Load.7 + pmin(Temp-15,0)
                 + toy*Temp + toy*Load.1
                 + Temp_s99_min
                 + WeekDays*Load.1
                ,data=cross.train)
    pred.lm.train = predict(modelLM,dplyr::select(train,-'Load'))
    pred.lm.test = predict(modelLM,test)
    pred.lm.rmse = caret::RMSE(predict(modelLM, cross.test), cross.test$Load)
    if(plt){
        plot(train$Load, type='l', xlim=c(0,length(total.time)),
             main="Prédiction du modèle linéaire", xlab="Temps", ylab="Load",
             sub=paste("RMSE =",pred.lm.rmse))
        lines(train$time, pred.lm.train, col='red', lwd=1)
        lines(test$time, pred.lm.test, col='green', lwd=1)
    }
    return(list("pred.train" = pred.lm.train, "pred.test" = pred.lm.test, "rmse" = pred.lm.rmse))
}
