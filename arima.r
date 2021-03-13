arima_rte = function(train_label, test_label, Date, plt = FALSE){
    train_ts = xts(train_label, order.by = Date)
    train_msts = msts(train_ts, seasonal.periods = 365.25)
    tmp = forecast::fourier(train_msts, K = 15, h = length(test_label))
    arimamodel = auto.arima(train_ts, seasonal =FALSE, xreg = forecast::fourier(train_msts, K  = 15))
    forecast = data.frame(forecast(arimamodel, xreg = tmp))
    if (plt){
        plot(forecast$Point.Forecast,type='l')
    }

    return(list("pred.train" = arimamodel$fitted, "pred.test" = forecast$Point.Forecast))
}
