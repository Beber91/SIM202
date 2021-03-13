lasso = function(cross.train, cross.test, train, test, plt = FALSE){
    lasso = cv.glmnet(data.matrix(select(cross.train,-'Load')),cross.train$Load,
                  family="gaussian",type.measure="mse",
                  lambda=10^seq(2,-3,by=-.1))

    lambda.min = lasso$lambda.min

    pred.lasso.train = predict(lasso,data.matrix(select(train,-'Load'))
                           ,s=lambda.min)
    pred.lasso.test = predict(lasso,data.matrix(test),s=lambda.min)

    rmse(pred.lasso.test[-N], to.test)

    return(list("pred.train" = pred.lasso.train, "pred.test" = pred.lasso.test))
}
