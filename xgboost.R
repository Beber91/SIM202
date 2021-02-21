xgboost_rte = function(train_set, train_label, test_set){
  param = list(booster = "gblinear", objective = "reg:squarederror", eval_metric = "rmse", lambda = 0.0003, alpha = 0.0003, nthread = 2, eta = 0.1)
  
  print("Model : XGBOOST")
  
  xgbmodel = xgboost(data = train_set, label = train_label, nrounds = 200, params = param, verbose = 0)
  
  pred = predict(xgbmodel, test_set)
  
  return(pred)
}
