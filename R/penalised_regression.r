#' @export
lasso = function(cross.train, cross.test, train, test, plt = FALSE){
    print("Lasso")
    lasso = glmnet::cv.glmnet(data.matrix(dplyr::select(cross.train,-'Load')),cross.train$Load,
                  family="gaussian",type.measure="mse",
                  lambda=10^seq(2,-3,by=-.1))

    lambda.min = lasso$lambda.min

    pred.lasso.train = predict(lasso,data.matrix(dplyr::select(train,-'Load'))
                           ,s=lambda.min)
    pred.lasso.test = predict(lasso,data.matrix(test),s=lambda.min)
    pred.lasso.rmse = caret::RMSE(predict(lasso, data.matrix(dplyr::select(cross.test,-"Load")),s=lambda.min),
     cross.test$Load)
    if(plt){
        plot(train$Load, type='l', xlim=c(0,length(total.time)),
             main="Prédiction de Lasso", xlab="Temps", ylab="Load",
             sub=paste("RMSE =",RMSE(predict(lasso, data.matrix(select(cross.test,-"Load")),s=lasso.lambda.min), cross.test$Load)))
        lines(train$time, pred.lasso.train, col='red', lwd=1)
        lines(test$time, pred.lasso.test, col='green', lwd=1)
    }
    return(list("pred.train" = pred.lasso.train, "pred.test" = pred.lasso.test, "rmse" = pred.lasso.rmse))
}

#' @export
ridge = function(cross.train, cross.test, train, test, plt = FALSE){
    print("Ridge")
    ridge = glmnet::cv.glmnet(data.matrix(dplyr::select(cross.train,-'Load')),cross.train$Load,
                    family="gaussian",type.measure="mse",nlambda=25)

    lambda.min = ridge$lambda.min

    pred.ridge.train = predict(ridge,data.matrix(dplyr::select(train,-'Load'))
                             ,s=lambda.min)
    pred.ridge.test = predict(ridge,data.matrix(test),s=lambda.min)
    pred.ridge.rmse = caret::RMSE(predict(ridge, data.matrix(dplyr::select(cross.test,-"Load")),s=lambda.min),
    cross.test$Load)

    if(plt){
        plot(train$Load, type='l', xlim=c(0,length(total.time)),
             main="Prédiction de Ridge", xlab="Temps", ylab="Load",
             sub=paste("RMSE =",RMSE(predict(ridge, data.matrix(select(cross.test,-"Load")),s=ridge.lambda.min), cross.test$Load)))
        lines(train$time, pred.ridge.train, col='red', lwd=1)
        lines(test$time, pred.ridge.test, col='green', lwd=1)
    }
    return(list("pred.train" = pred.ridge.train, "pred.test" = pred.ridge.test, "rmse" = pred.ridge.rmse))
}

#' @export
elastic = function(cross.train, cross.test, train, test, plt = FALSE){
    print("Elastic Network")
    elastic_net = glmnet::cv.glmnet(data.matrix(dplyr::select(cross.train,-'Load')),cross.train$Load,
          family="gaussian",type.measure="mse",alpha=0.7)

    lambda.min = elastic_net$lambda.min
    pred.elastic.train = predict(elastic_net,data.matrix(dplyr::select(train,-'Load'))
                             ,s=lambda.min)
    pred.elastic.test = predict(elastic_net,data.matrix(test),s=lambda.min)
    pred.elastic.rmse = caret::RMSE(predict(elastic_net, data.matrix(dplyr::select(cross.test,-"Load")), s=lambda.min), cross.test$Load)
    if(plt){
        plot(train$Load, type='l', xlim=c(0,length(total.time)),
             main="Prédiction d'Elastic Net", xlab="Temps", ylab="Load",
             sub=paste("RMSE =",RMSE(predict(elastic, data.matrix(select(cross.test,-"Load")),s=elastic.lambda.min), cross.test$Load)))
        lines(train$time, pred.elastic.train, col='red', lwd=1)
        lines(test$time, pred.elastic.test, col='green', lwd=1)
    }
    return(list("pred.train" = pred.elastic.train, "pred.test" = pred.elastic.test, "rmse" = pred.elastic.rmse))
}
