gam_rte = function(cross.train, cross.test, train, test, plt = FALSE){
    model = gam(Load~s(Load.1,k=6)+s(Load.7,k=7)+s(Temp,k=6,bs="cr")
                +s(WeekDays,k=7)+s(toy,k=6,bs="cr")
                +ti(toy,Temp,bs="cr")+ti(toy,Load.1,bs="cr")
                +ti(toy,Temp_s99_min,k=6,bs="cr")
                +ti(WeekDays,Load.1,k=6)
               ,data=cross.train)
    #summary(model)
    
    pred.gam.train = predict(model,select(train,-'Load'))
    pred.gam.test = predict(model,test)
    
    #N = length(test$Load.1)
    #RMSE = rmse(pred.gam.test[-N], test) #on connait pas le dernier
    #print(RMSE)
    return(list("pred.train" = pred.gam.train, "pred.test" = pred.gam.test))
}
