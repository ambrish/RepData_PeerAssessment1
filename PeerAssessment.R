# Libraries for plotting (ggplot2) and transforming data (plyr).
# Reading the data 
library(ggplot2)
library(plyr)

# download and read the data
data <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))

byDay <- aggregate(steps ~ date, data, sum, na.action = na.pass)
# Because we wanna to track this information, we add a label
byDay <- cbind(byDay, label = rep("with.na", nrow(byDay)))
ggplot(byDay, aes(x = steps)) + geom_histogram(binwidth = 1500, colour = "black", 
    fill = "white") + labs(title = "Steps Taken per Day", x = "Number of Steps", 
    y = "Frequency")
    
#Imputing missing values
    data.impute <- adply(data, 1, function(x) if (is.na(x$steps)) {
    x$steps = round(byInterval[byInterval$interval == x$interval, 2])
    x
} else {
    x
})
# Because we wanna to track this information, we add a label
byDay.impute <- aggregate(steps ~ date, data.impute, sum)
byDay.impute <- cbind(byDay.impute, label = rep("without.na", nrow(byDay.impute)))
ggplot(byDay.impute, aes(x = steps)) + geom_histogram(binwidth = 1500, colour = "black", 
    fill = "white") + labs(title = "Steps Taken per Day", x = "Number of Steps", 
    y = "Frequency")

byDay.all <- rbind(byDay, byDay.impute)
levels(byDay.all$label) <- c("With NA", "Without NA")
ggplot(byDay.all, aes(x = steps, fill = label)) + geom_histogram(binwidth = 1500, 
    colour = "black", alpha = 0.2) + labs(title = "Steps Taken per Day", x = "Number of Steps", 
    y = "Frequency") + theme(legend.position = "bottom")
    
    
# For some problems in system time
Sys.setlocale(locale = "C")

# We obtain the two subsets
data.weekend <- subset(data.impute, weekdays(date) %in% c("Saturday", "Sunday"))
data.weekday <- subset(data.impute, !weekdays(date) %in% c("Saturday", "Sunday"))

# Obtain the average steps per interval for each dataset
data.weekend <- aggregate(steps ~ interval, data.weekend, mean)
data.weekday <- aggregate(steps ~ interval, data.weekday, mean)

# By plotting we add a label
data.weekend <- cbind(data.weekend, day = rep("weekend"))
data.weekday <- cbind(data.weekday, day = rep("weekday"))
# Combine the subsets and a specify the levels
data.week <- rbind(data.weekend, data.weekday)
levels(data.week$day) <- c("Weekend", "Weekday")

ggplot(data.week, aes(x = interval, y = steps)) + geom_line() + facet_grid(day ~ 
    .) + labs(x = "Interval", y = "Number of steps")
