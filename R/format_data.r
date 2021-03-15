#' @export
format_data = function(file_train, file_test) {

    print(paste("Load and format " , file_train, sep = " "))
    train_set = readr::read_csv(file_train, col_types = readr::cols())

    train_set$WeekDays = days_to_numeric(train_set)

    train_set$Year = NULL
    train_set$Date = NULL
    train_label = data.matrix(train_set$Load)


    train_set$Load = NULL

    train_set = data.matrix(train_set)


    print(paste("Load and format " , file_test, sep = " "))
    test_set = readr::read_csv(file_test, col_types = readr::cols())

    test_set$WeekDays = days_to_numeric(test_set)

    test_label = test_set$Load.1
    tmp = test_set$Load.1
    for (i in c(1:(length(tmp)-1))){
        test_label[i] = tmp[i+1]
    }

    test_set$Year = NULL
    test_set$Date = NULL
    test_set$Id = NULL
    test_set$Usage = NULL
    test_set = data.matrix(test_set)


    test_set = data.matrix(test_set)
    return(list("train_set" = train_set, "train_label" = train_label, "test_set" = test_set, "test_label" = test_label))
}

#' @export
format_data_mat = function(path_train, path_test){
    invisible(train <- readr::read_delim(file=path_train,delim=','))
    invisible(test <- readr::read_delim(file=path_test,delim=','))

    train$WeekDays = days_to_numeric(train)
    test$WeekDays = days_to_numeric(test)

    train$Year = train$Year - 2012
    test$Year = test$Year - 2012

    total.time = c(1:(nrow(train)+nrow(test)))
    length(total.time)
    train$time = total.time[1:nrow(train)]
    test$time = tail(total.time,nrow(test))

    return(list("train" = train, "test" = test))
}

#' @export
scdat = function(train, test){
    summary(train)
    tr = dplyr::select(train,-c('Date'))
    te = dplyr::select(test,-c('Date','Usage','Id'))
    list("train" = tr, "test" = te)
}
