# Libraries for plotting (ggplot2) and transforming data (plyr).
# Reading the data 
library(ggplot2)
library(plyr)

# download and read the data
data <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
