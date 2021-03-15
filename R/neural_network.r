#' @export
neural_network = function(train, test, plt = FALSE){
    labels = select(train,'Load')
    train.lstm = train
    test.lstm = test

    lstm = function(train_set, train_label, test_set) {
        ##y = train_label
        ##x = data.matrix(train_set)
        ##x_test = data.matrix(test_set)

        #nombre de lignes par sample du batch
        window = nrow(test_set)
        n_windows = nrow(train_set) - window + 1
        n_features = ncol(train_set)

        x_train = array(data=NA, dim=c(n_windows,window,n_features))
        y_train = array(data=NA, dim=c(n_windows,window,1))

        for (i in window:nrow(train_set)) {
            x_train[i-window+1,,] = data.matrix(train_set[(i-window+1):i,])
            y_train[i-window+1,,] = as.matrix(data.matrix(train_label[(i-window+1):i,]))
        }

        x_test = array(data=NA, dim=c(1,window,n_features))
        for (i in window:nrow(test_set)) {
            x_test[i-window+1,,] = data.matrix(test_set[(i-window+1):i,])
        }

        batch_size = 40

        ## La taille du training set doit etre un multiple du nombre de batchs
        ## On modifie le testing set en consequence
        n = size(x_train)[1]
        to_remove = sample.int(n, n %% batch_size)

        x_train = x_train[-to_remove,,]

        y_train = y_train[-to_remove,,]
        y_train = array(y_train,dim=c(size(y_train)[1],size(y_train)[2],1))

        model = keras_model_sequential() %>%
            layer_lstm(
                units = 64,
                batch_input_shape = c(batch_size, window, n_features),
                return_sequences = T,
                activation=layer_activation_relu(max_value = 40000),
                kernel_regularizer=regularizer_l2(1e-3),
                bias_regularizer=regularizer_l2(1e-3)
            ) %>%
            layer_lstm(
                units = 64,
                return_sequences = T,
                activation=layer_activation_relu(max_value = 40000),
                kernel_regularizer=regularizer_l2(1e-3),
                bias_regularizer=regularizer_l2(1e-3)
            ) %>%
            layer_lstm(
                units = n_features,
                return_sequences = T,
                activation=layer_activation_relu(max_value = 40000),
                kernel_regularizer=regularizer_l2(1e-3),
                bias_regularizer=regularizer_l2(1e-3)
            ) %>%
            time_distributed(layer_dense(units = 1))

        model %>% compile(loss = 'mse',optimizer_rmsprop(
            lr = 0.01,
            rho = 0.9,
            epsilon = NULL,
            decay = 0,
            clipnorm = 1,
            clipvalue = NULL
        ))

        model %>% fit(x_train, y_train, epochs=4, batch_size=batch_size)
        print("www")
        ## On test sur un batch_size de 1
        model_evaluate = keras_model_sequential() %>%
            layer_lstm(
                units = 64,
                batch_input_shape = c(1, window, n_features),
                return_sequences = T,
                activation=layer_activation_relu(max_value = 40000),
                kernel_regularizer=regularizer_l2(1e-3),
                bias_regularizer=regularizer_l2(1e-3)
            ) %>%
            layer_lstm(
                units = 64,
                return_sequences = T,
                activation=layer_activation_relu(max_value = 40000),
                kernel_regularizer=regularizer_l2(1e-3),
                bias_regularizer=regularizer_l2(1e-3)
            ) %>%
            layer_lstm(
                units = n_features,
                return_sequences = T,
                activation=layer_activation_relu(max_value = 40000),
                kernel_regularizer=regularizer_l2(1e-3),
                bias_regularizer=regularizer_l2(1e-3)
            ) %>%
            time_distributed(layer_dense(units = 1))

        print("www1")
        ## On copie le model pre-entraine
        set_weights(model_evaluate, get_weights(model))
        print("www2")
        pred.test = model_evaluate %>%
            predict(x_test, batch_size = 1) %>%
            .[, , 1]
        print("www4")

        return(list("train"=1,"test"=pred.test,
                    "model"=model_evaluate))
    }


    pred.lstm = lstm(train.lstm, labels, test.lstm)

    if (plt){
        par(mfrow=c(1,1))
        plot(train$Load,type='l', xlim=c(0,length(total.time)))
        lines(test$time,pred.lstm$test, col='green', lwd=1)
    }
    return(pred.lstm)
}
