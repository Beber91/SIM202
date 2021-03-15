#' @export
arima_rte = function(train_label, test_label, Date, plt = FALSE){
    train_ts = xts::xts(train_label, order.by = Date)
    train_msts = forecast::msts(train_ts, seasonal.periods = 365.25)
    tmp = forecast::fourier(train_msts, K = 15, h = length(test_label))
    arimamodel = forecast::auto.arima(train_ts, seasonal =FALSE, xreg = forecast::fourier(train_msts, K  = 15))
    forecast = data.frame(forecast::forecast(arimamodel, xreg = tmp))
    if (plt){
        plot(train$Load, type='l', xlim=c(0,length(total.time)),
             main="Prédiction de ARIMA", xlab="Temps", ylab="Load")
        lines(test$time, forecast$Point.Forecast, col='green', lwd=1)
    }
    return(list("pred.train" = arimamodel$fitted, "pred.test" = forecast$Point.Forecast))
}
