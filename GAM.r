gam_rte = function(train, test, plt = FALSE){
    Gam <- gam(Load~s(Load.1)+s(Load.7)+s(Temp)
               +s(Temp_s95)+s(WeekDays,k=7)
               +s(GovernmentResponseIndex)
              ,data=train)
    summary(Gam)


    gam.train = predict(Gam, newdata=train)
    gam.test = predict(Gam, newdata=test)

    if (plt){
        par(mfrow=c(1,1))
        plot(train$Load,type='l', xlim=c(0,length(total.time)))
        lines(train$time,Gam$fit, col='red', lwd=1)
        lines(test$time,gam.test, col='green', lwd=1)
    }

    return(gam.test)
}
