#' @export
SVM = function(cross.train, cross.test, train, test, plt = FALSE){
  print("SVM")
  optimise.svm = e1071::tune(e1071::svm, Load ~ toy + Temp + Temp_s99_min +
               Load.1 + Load.7 + WeekDays, data=cross.train,
             ranges=list(epsilon=seq(0,1,0.1), cost=1:5))

  SVM = optimise.svm$best.model

  pred.svm.test = predict(SVM, test)
  pred.svm.train = predict(SVM, train)
  pred.svm.rmse = caret::RMSE(predict(SVM, cross.test), cross.test$Load)
  if (plt){
    plot(train$Load, type='l', xlim=c(0,length(total.time)),
         main="Prédiction de la SVM", xlab="Temps", ylab="Load",
         sub=paste("RMSE =",RMSE(pred.svm.rmse)))
    lines(train$time, pred.svm.train, col='red', lwd=1)
    lines(test$time, pred.svm.test, col='green', lwd=1)
  }
   return(list("pred.train" = pred.svm.train, "pred.test" = pred.svm.test, "rmse" = pred.svm.rmse))
}
