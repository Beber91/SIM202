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

  return(list("train"=cross.train,"test"=cross.test))
}
