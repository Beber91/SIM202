SVM = function(cross.train, cross.test, train, test, plt = FALSE){
  
  aaa = tune(svm, Load ~ toy + Temp + Temp_s99_min +
               Load.1 + Load.7 + WeekDays, data=cross.train,
             ranges=list(epsilon=seq(0,1,0.1), cost=1:5))
  
  SVM = aaa$best.model
  
  pred.svm.test = predict(SVM, test)
  pred.svm.train = predict(SVM, train)
  
  #rmse(pred.svm.train, train$Load)
  #rmse(pred.svm.test[-N], test)
  
  if (plt){
    par(mfrow=c(1,1))
    plot(train$Load,type='l', xlim=c(0,length(total.time)))
    lines(train$time,pred.svm.train, col='red', lwd=1)
    lines(test$time,pred.svm.test, col='green', lwd=1)
  }
   return(list("pred.train" = pred.svm.train, "pred.test" = pred.svm.test))
}
