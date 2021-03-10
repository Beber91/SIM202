neural_network = function(train, test, plt = FALSE){
  labels = train[,2]
  train.lstm = train[,-c(1,2,22,23)]
  test.lstm = test[,-c(1,20,21,23,24)]
  
  lstm = function(train_set, train_label, test_set) {
    ##y = train_label
    ##x = data.matrix(train_set)
    ##x_test = data.matrix(test_set)
    
    #nombre de lignes par sample du batch
    window = 10
    n_windows = nrow(train_set) - window + 1
    
    n_features = ncol(train_set)
    
    
    x = array(data=NA, dim=c(n_windows,
                             window,
                             n_features))
    
    y = array(data=NA, dim=c(n_windows,
                             window,
                             1))
    
    for (i in 1:n_windows) {
      x[i,,] = data.matrix(train_set[i:(i + window - 1),])
      y[i,,] = as.matrix(data.matrix(train_label[i:(i + window - 1),]))
    }
    i = 1
    print(size(y))
    
    batch_size = 1
    
    lrelu = function(x) tf.keras.activations.relu(x, alpha=0.1)
    model = keras_model_sequential() %>%
      layer_lstm(
        units = 64,
        batch_input_shape = c(batch_size, window, n_features),
        dropout = 0.2,
        recurrent_dropout = 0.2,
        return_sequences = T,
      ) %>%
      time_distributed(layer_dense(units = 1))
    
    model %>% compile(loss = 'mae',
                      optimizer = optimizer_rmsprop())
    
    model %>% fit(x, y, epochs=15)
    
    pred.train = model %>% predict(X_train)
    pred.test = model %>% predict(X_test)
    
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
