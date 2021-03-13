elastic = function(cross.train, cross.test, train, test, plt = FALSE){
    elastic_net = cv.glmnet(data.matrix(select(cross.train,-'Load')),cross.train$Load,
          family="gaussian",type.measure="mse",alpha=0.7)
    lambda.min = elastic_net$lambda.min
    pred.elastic.train = predict(elastic_net,data.matrix(select(train,-'Load'))
                             ,s=lambda.min)
    pred.elastic.test = predict(elastic_net,data.matrix(test),s=lambda.min)

    #rmse(pred.elastic.test[-N], to.test)
    return(list("pred.train" = pred.elastic.train, "pred.test" = pred.elastic.test))
}
