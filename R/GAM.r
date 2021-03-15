#' @export
gam_rte = function(cross.train, cross.test, train, test, plt = FALSE){
    print("GAM")
    model = mgcv::gam(Load~s(Load.1,k=6)+s(Load.7,k=7)+s(Temp,k=6,bs="cr")
                +s(WeekDays,k=7)+s(toy,k=6,bs="cr")
                +ti(toy,Temp,bs="cr")+ti(toy,Load.1,bs="cr")
                +ti(toy,Temp_s99_min,k=6,bs="cr")
                +ti(WeekDays,Load.1,k=6)
               ,data=cross.train)
    #summary(model)

    pred.gam.train = predict(model,dplyr::select(train,-'Load'))
    pred.gam.test = predict(model,test)
    pred.gam.rmse = caret::RMSE(predict(model, cross.test), cross.test$Load)

    if(plt){
        plot(train$Load, type='l', xlim=c(0,length(total.time)),
             main="Prédiction du GAM", xlab="Temps", ylab="Load",
             sub=paste("RMSE =",pred.gam.rmse))
        lines(train$time, pred.gam.train, col='red', lwd=1)
        lines(test$time, pred.gam.test, col='green', lwd=1)
    }
    return(list("pred.train" = pred.gam.train, "pred.test" = pred.gam.test, "rmse" = pred.gam.rmse))
}
