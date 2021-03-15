#' @export
fourier = function(train, test, plt = FALSE){
    total.time = c(1:(nrow(train)+nrow(test)))
    length(total.time)
    train$time = total.time[1:nrow(train)]
    test$time = tail(total.time,nrow(test))

    fourier.make.matrix = function (t, k, period) {
        w = 2*pi/period
        ret = cbind(cos(w*t), sin(w*t))

        for(i in c(2:K))
        {
            ret = cbind(ret, cos(i*w*t), sin(i*w*t))
        }
        return (ret)
    }

    K = 5; period = 365
    fourier.train = fourier.make.matrix(train$time, K, period)
    fourier.test = fourier.make.matrix(test$time, K, period)

    fourier.train.df = data.frame(train$Load,fourier.train)
    fourier.test.df = data.frame(fourier.test)

    reg = lm(train.Load ~., data=fourier.train.df)

    pred.fourier = predict(reg, newdata=fourier.test.df)
    total.fourier = c(reg$fitted,pred.fourier)
    if (plt){
        par(mfrow=c(1,1))
        plot(train$Load,type='l', xlim=c(0,length(total.time)))
        lines(reg$fitted,col='red', lwd=2)
        lines(test$time,pred.fourier,col='green', lwd=2)
    }
    return(pred.fourier)
}
