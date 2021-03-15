###Analyse Descriptive des données

##Month average
#' @export
month_average = function(ts){
    ylabel = dimnames(ts)
    ylabel = ylabel[[2]][[1]]
    month = as.factor(.indexmon(ts))
    mean.month = tapply(ts, month, mean)
    plot(mean.month, type = "b", xlab = "Month", ylab = ylabel)
}

##Day of the week average
#' @export
dow_average = function(ts){
    ylabel = dimnames(ts)
    ylabel = ylabel[[2]][[1]]
    dow = as.factor(.indexwday(ts))
    concentration_day = tapply(ts, dow, mean)
    plot(concentration_day, type = "b", xlab = "Day of the week", ylab = ylabel)
}

##Hourly average
#' @export
hour_average = function(ts, ylabel){
    ylabel = dimnames(ts)
    ylabel = ylabel[[2]][[1]]
    hour = as.factor(.indexhour(ts))
    mean.dow.hour = tapply(ts, hour, mean)
    plot(mean.dow.hour, type = "b", xlab = "Hourly profile", ylab = ylabel)
}


## Noise
#' @export
get_noise = function(ts){
    month = as.factor(.indexmon(ts))
    mean.month = tapply(ts, month, mean)
    noise = c()
    for (i in c(1:length(ts))){
        noise = c(noise, mean.month[month[i]]- ts[i])
    }
    return(noise)
}

#' @export
anal_matthieu = function(train, plt = FALSE){
    ##annual seasonality
    if (plt){
        plot(train$Date, train$Load, type='l')
    }

    MA <- stats::filter(train$Load, filter = rep(1/365,365),
                        method = c("convolution"), sides = 2, circular = FALSE)
    if (plt){
        plot(train$Date, train$Load, type = "l", xlab = "",
             ylab = "consumption (kw)", col = "seagreen4", lwd = 1)
        lines(train$Date, MA, col = "red", lwd = 2)
    }

    ##week seasonality
    num.years = 0
    average = 0
    N = 8; a = 0.7 #exponential weight
    while (365*(num.years+1) <= length(train$Date)) {
        par(mfrow = c(1, 1))
        dateyear = train$Date[(365*num.years+1):(365*(num.years+1))]
        loadyear = train$Load[(365*num.years+1):(365*(num.years+1))]

        ##plot(dateyear,loadyear,type='l')

        MAw <- stats::filter(loadyear, filter = rep(1/52,52),
                             method = c("convolution"), sides = 2, circular = T)
        ##plot(dateyear,loadyear, type = "l", xlab = "",
        ##    ylab = "consumption (kw)", col = "seagreen4", lwd = 1)
        ##lines(dateyear, MAw, col = "red", lwd = 2)

        ##plot(dateyear, loadyear - MAw, type="l")
        expweight = (((1-a)/(1-a^8))*a^(N-(num.years+1)))
        average = expweight*(average + (loadyear - MAw))
        num.years = num.years + 1
    }
    if (plt){
        plot(average)
    }
    return(average)
}
