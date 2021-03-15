#' @export
cross_validation = function (data) {
  set.seed(123)

  to.train = mc2d::rbern(n=length(data$Load),p=0.8)==T

  cross.train =  data[to.train, ]
  cross.test  =  data[!to.train, ]

  return(list("train"=cross.train,"test"=cross.test))
}
