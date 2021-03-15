days_to_numeric = function (data) {
    data$WeekDays[data$WeekDays=='Monday']    = 1
    data$WeekDays[data$WeekDays=='Tuesday']   = 2
    data$WeekDays[data$WeekDays=='Wednesday'] = 3
    data$WeekDays[data$WeekDays=='Thursday']  = 4
    data$WeekDays[data$WeekDays=='Friday']    = 5
    data$WeekDays[data$WeekDays=='Saturday']  = 6
    data$WeekDays[data$WeekDays=='Sunday']    = 7
    data$WeekDays = as.numeric(data$WeekDays)
    return (data$WeekDays)
}
