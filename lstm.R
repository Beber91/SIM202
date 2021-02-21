lstm = function(train_set, train_label, test_set){
  N = length(train_set[,'Load.1'])
  nb_var = 17
  y = train_label
  x = array(train_set, dim = c(N, nb_var, 1))
  x_test = array(test_set,dim = c(length(test_set[,'Load.1']), nb_var, 1))
  print("step1")
  model = keras_model_sequential() %>%   
    #layer_lstm(units=128, input_shape=c(nb_var, 1), activation="relu", return_sequences = TRUE) %>% 
    layer_dense(units=64, activation = "relu") %>%  
    layer_dense(units=32) %>%  
    layer_dense(units=1, activation = "linear")
  print("step2")
  model %>% compile(loss = 'mse',
                    optimizer = 'adam',
                    metrics = list("mean_absolute_error")
  )
  print("step3")
  model %>% fit(x,y, epochs=100, batch_size=32, shuffle = FALSE)
  print("step4")
  print(length(x_test))
  pred = model %>% predict(x_test)
  print("step5")
  return(pred)
    
}
