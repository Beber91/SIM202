cross_validation = function (train, test) {
  set.seed(123)
  to.extract = rbern(n=length(train$Load),p=0.8)==T
  to.train = train[F,]
  for (j in 1:length(to.extract)) {
    if (to.extract[j]) {
      to.train[nrow(to.train)+1,] = train[j,]
    }
  }
  
  cross.train  <- train[to.train$time, ]
  cross.test <- train[-to.train$time, ]
  
  model = gam(Load~s(Load.1)+s(Load.7)+s(Temp)
               +s(Temp_s95)+s(WeekDays,k=7)
               ,data=cross.train)
  
  pred.test = predict(model,test)
  print(length(pred.test))
  N = length(test$Load.1)
  RMSE = rmse(pred.test[-N], test$Load.1[2:N]) #on connait pas le dernier
  
  print(RMSE)
  return(list("model"=model,"pred.test"=pred.test,"RMSE"=RMSE))
}
