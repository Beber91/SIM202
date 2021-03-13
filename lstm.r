lstm = function(train, test, plt = FALSE){
  labels = train[,'Load']
  train.lstm = select(train,-'Load')
  test.lstm = test
  
  Neural.net = function(train_set, train_label, test_set) {
    y_train = train_label
    x_train = data.matrix(train_set)
    x_test = data.matrix(test_set)
    
    print(size(x_train))
    model = keras_model_sequential() %>% 
      layer_dense(units=64, activation="relu", input_shape=c(19)) %>% 
      layer_dense(units=32, activation = "relu") %>% 
      layer_dense(units=1, activation="linear")
    
    model %>% compile(
      loss = "mse",
      optimizer =  "adam", 
      metrics = list("mean_absolute_error")
    )
    ##summary(model)
    model %>% fit(x_train, y_train, epochs=4)
    
    pred.train = model %>% predict(x_train)
    pred.test = model %>% predict(x_test)
    
    return(list("train"=pred.train,"test"=pred.test,
                "model"=model))
  }
  
  pred.lstm = Neural.net(train.lstm, labels, test.lstm)
  
  if (plt){
    par(mfrow=c(1,1))
    plot(train$Load,type='l', xlim=c(0,length(total.time)))
    lines(test$time,pred.lstm$test, col='green', lwd=1)
  }
  
  return(pred.lstm)
}