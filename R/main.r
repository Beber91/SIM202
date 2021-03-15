## A FAIRE SVM + ACF ET PCF SUR LOAD + SITE FAVORI + STACKING
main = function(){
  rm(list=objects())
  graphics.off()

  ##Environment var
  plt = F #(si on veut plot, mettre à TRUE)
  path_submission = "./submissions/submission.csv"
  path_train = system.file("extdata", "train_V2.csv", package = "SIM202")

  path_test = system.file("extdata", "test_V2.csv", package = "SIM202")

  ## Format data
  model = "aucun"
  date1 = strptime("01/01/2012", "%d/%m/%Y")
  date2 = strptime("16/04/2020", "%d/%m/%Y")
  Date = seq(date1, date2, by = "1 day")

  data = format_data(path_train, path_test)
  train_set = data$train_set
  test_set = data$test_set
  train_label = data$train_label
  test_label = data$test_label

  data_mat = format_data_mat(path_train, path_test)
  train = data_mat$train; test = data_mat$test

  ## Fourier

  pred.fourier = fourier(train, test, plt = plt)

  ## scale data

  data_sc = scdat(train, test)
  train = data_sc$train; test = data_sc$test

  ## cross-validation

  cv = cross_validation(train)
  cross.train = cv$train; cross.test = cv$test

  ## Linear Model

  pred.lm = linmod(cross.train, cross.test, train, test)
  pred.lm.train = pred.lm$pred.train; pred.lm.test = pred.lm$pred.test
  print(paste("RMSE = ", pred.lm$rmse))
  ## GAM

  pred.gam = gam_rte(cross.train, cross.test, train, test)
  pred.gam.train = pred.gam$pred.train; pred.gam.test = pred.gam$pred.test
  print(paste("RMSE = ", pred.gam$rmse))

  ## Random forest

  #randomForest::tuneRF(dplyr::select(train, -'Load'),train$Load)
  n_trees = 12
  pred.rf = random_forest(cross.train, cross.test, train, test, n_trees)
  pred.rf.train = pred.rf$pred.train ; pred.rf.test = pred.rf$pred.test
  print(paste("RMSE = ", pred.rf$rmse))

  ## SVM

  pred.svm = SVM(cross.train, cross.test, train, test)
  pred.svm.train = pred.svm$pred.train; pred.svm.test = pred.svm$pred.test
  print(paste("RMSE = ", pred.svm$rmse))

  ## Elastic-Net

  pred.elastic = elastic(cross.train, cross.test, train, test)
  pred.elastic.train = pred.elastic$pred.train; pred.elastic.test = pred.elastic$pred.test
  print(paste("RMSE = ", pred.elastic$rmse))

  ## Ridge

  pred.ridge = ridge(cross.train, cross.test, train, test)
  pred.ridge.train = pred.ridge$pred.train; pred.ridge.test = pred.ridge$pred.test
  print(paste("RMSE = ", pred.ridge$rmse))

  ## Lasso

  pred.lasso = lasso(cross.train, cross.test, train, test)
  pred.lasso.train = pred.lasso$pred.train; pred.lasso.test = pred.lasso$pred.test
  print(paste("RMSE = ", pred.lasso$rmse))

  ## ARIMA

  pred.arima = arima_rte(train_label, test_label, Date)
  pred.arima.train = pred.arima$pred.train; pred.arima.test = pred.arima$pred.test

  ## Gradient Boosting

  pred.xgb = xgboost_rte(cross.train, cross.test, train, test)
  pred.xgb.train = pred.xgb$pred.train; pred.xgb.test = pred.xgb$pred.test
  print(paste("RMSE = ", pred.xgb$rmse))

  ## Expert Agregation


  list_experts_train = abind::abind(pred.gam.train,
                             pred.rf.train,
                             pred.svm.train,
                             pred.elastic.train,
                             pred.ridge.train,
                             pred.lasso.train,
                             pred.arima.train,
                             pred.lm.train,
                             pred.xgb.train,
                             along = 2)

  experts.test = cbind(pred.gam.test,
                       pred.rf.test,
                       pred.svm.test,
                       pred.elastic.test,
                       pred.ridge.test,
                       pred.lasso.test,
                       pred.arima.test,
                       pred.lm.test,
                       pred.xgb.test)
  experts.train = add_expert(list_experts_train)

  oracle1 = opera::mixture(
              Y = train_label,
              experts = experts.train,
              model = "OGD",
              loss.type = "square",
              coefficients = "Uniform",
  )
  coeffs1 = oracle1$coefficients
  print(oracle1)

  pred.oracle = experts.test %*% coeffs1#c(0.4,0.4,0.1,0.1)##coeffs2
  if(plt){
      par(mfrow=c(1,1))
      plot(train_label,type='l', xlim=c(0,length(total.time)))
      lines(test$time,pred.oracle, col='green', lwd=1)
  }

  #write.csv(submission, file =path_submission, row.names=F)
}

