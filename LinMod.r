linmod = function(cross.train, cross.test, train, test, plt = FALSE){
    modelLM = lm(Load~Load.7 + pmin(Temp-15,0)
                 + toy*Temp + toy*Load.1
                 + Temp_s99_min
                 + WeekDays*Load.1
                ,data=cross.train)
    summary(modelLM)
    pred.lm.train = predict(modelLM,select(train,-'Load'))
    pred.lm.test = predict(modelLM,test)
    if(plt){
        plot(train$Temp,train$Load)
    }
    return(list("pred.train" = pred.lm.train, "pred.test" = pred.lm.test))
}
