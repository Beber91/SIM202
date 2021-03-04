neural_network = function(train, test, plt = FALSE){
    labels = train$Load
    train.lstm = train[,-c(1,2,22,23)]
    test.lstm = test[,-c(1,20,21,23,24)]

    lstm = function(train_set, train_label, test_set) {
        y = train_label
        x = data.matrix(train_set)
        x_test = data.matrix(test_set)
        model = keras_model_sequential() %>%
            layer_dense(units = 128, activation = 'relu', input_shape = c(19)) %>%
            layer_dense(units = 64, activation = 'relu') %>%
            layer_dense(units = 32, activation = 'relu') %>%
            layer_dense(units = 16, activation = 'relu') %>%
            layer_dense(units=1, activation = "linear")
        
        model %>% compile(loss = 'mse',
                          optimizer = optimizer_adam(0.0005))
        
        model %>% fit(x,y, epochs=15)

        pred.train = model %>% predict(x)
        pred.test = model %>% predict(x_test)

        return(list("train"=pred.train,"test"=pred.test,"model"=model))
    }


    pred.lstm = lstm(train.lstm, labels, test.lstm)

    if (plt){
        par(mfrow=c(1,1))
        plot(train$Load,type='l', xlim=c(0,length(total.time)))
        lines(test$time,pred.lstm$test, col='green', lwd=1)
    }

    N = length(test$Load.1)
    RMSE = rmse(pred.lstm$test[-N], test$Load.1[2:N])
    RMSE
    return(pred.lstm)
}
