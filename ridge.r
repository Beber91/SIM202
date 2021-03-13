ridge = function(cross.train, cross.test, train, test, plt = FALSE){
    ridge = cv.glmnet(data.matrix(select(cross.train,-'Load')),cross.train$Load,
                    family="gaussian",type.measure="mse",nlambda=25)

    lambda.min = ridge$lambda.min

    pred.ridge.train = predict(ridge,data.matrix(select(train,-'Load'))
                             ,s=lambda.min)
    pred.ridge.test = predict(ridge,data.matrix(test),s=lambda.min)

    return(list("pred.train" = pred.ridge.train, "pred.test" = pred.ridge.test))
}

