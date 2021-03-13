format_data = function(file_train, file_test) {

    print(paste("Load and format " , file_train, sep = " "))
    train_set = read_csv(file_train, col_types = cols())

    train_set$WeekDays[train_set$WeekDays == "Monday"] <- 1
    train_set$WeekDays[train_set$WeekDays == "Tuesday"] <- 2
    train_set$WeekDays[train_set$WeekDays == "Wednesday"] <- 3
    train_set$WeekDays[train_set$WeekDays == "Thursday"] <- 4
    train_set$WeekDays[train_set$WeekDays == "Friday"] <- 5
    train_set$WeekDays[train_set$WeekDays == "Saturday"] <- 6
    train_set$WeekDays[train_set$WeekDays == "Sunday"] <- 7
    train_set$WeekDays = as.integer(train_set$WeekDays)

    train_set$Year = NULL
    train_set$Date = NULL
    train_label = data.matrix(train_set$Load)


    train_set$Load = NULL
    
    train_set = data.matrix(train_set)


    print(paste("Load and format " , file_test, sep = " "))    
    test_set = read_csv(file_test, col_types = cols())

    test_set$WeekDays[test_set$WeekDays == "Monday"] <- 1
    test_set$WeekDays[test_set$WeekDays == "Tuesday"] <- 2
    test_set$WeekDays[test_set$WeekDays == "Wednesday"] <- 3
    test_set$WeekDays[test_set$WeekDays == "Thursday"] <- 4
    test_set$WeekDays[test_set$WeekDays == "Friday"] <- 5
    test_set$WeekDays[test_set$WeekDays == "Saturday"] <- 6
    test_set$WeekDays[test_set$WeekDays == "Sunday"] <- 7
    test_set$WeekDays = as.integer(test_set$WeekDays)

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

format_data_mat = function(path_train, path_test){
    train <- read_delim(file=path_train,delim=',')
    test <- read_delim(file=path_test,delim=',')

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

scdat = function(train, test){
    summary(train)
    tr = select(train,-c('Date'))
    te = select(test,-c('Date','Usage','Id'))
    list("train" = tr, "test" = te)
}
