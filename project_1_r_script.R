setwd("C:/Users/bdfitzgerald/Desktop/Data Science Specialist/RepData_PeerAssessment1")

## Loading and preprocessing the data
        ## Question 1
unzip("activity.zip")
data <- read.csv("activity.csv")
options(scipen = 1)  # Turn off scientific notations for numbers
        ## Question 2
data$date <- as.Date(data$date) ## reclass date as a Date
sapply(data, class) ## see the class of the data
head(data, 3) ## Views the first three rows
summary(data)


## What is mean total number of steps taken per day?
num_nas <- sum(is.na(data))
        ## Question 1
steps.taken <- aggregate(data$steps, by = list(Date = data$date), sum, na.rm = TRUE)
colnames(steps.taken)[2] <- "num_steps"
library("ggplot2")
steps <- ggplot(steps.taken, aes(Date, num_steps)) + 
        geom_bar(stat = "identity", colour = "green4", fill = "green4", width = 1) + 
        labs(title = "Total Number of Steps Taken by Day", 
             x = "Date", y = "Number of Steps by Taken per Day") 
        ## Question 2
steps.mean <- round(mean(steps.taken$num_steps, na.rm = TRUE), 2) ## Round to two decimals 
steps.median <- median(steps.taken$num_steps, na.rm = TRUE)

## What is the average daily activity pattern?
        ## Question 1
avg.steps.taken <- aggregate(data$steps, by = list(intervals = data$interval), mean, na.rm = TRUE) 
colnames(avg.steps.taken)[2] <- "avg_steps"
avg.steps <- ggplot(avg.steps.taken, aes(x = intervals, y = avg_steps)) +
        geom_line(color = "green4", size = 0.75) +
        ylim(0, max(avg.steps.taken$avg_steps)) +
        xlab("Five-minute Intervals") +
        ylab("Average Number of Steps") +
        ggtitle("Title Series Plot of Five-minute Intervals") 
avg.steps
        ## Question 2
ast.interval <- avg.steps.taken[avg.steps.taken$avg_steps == max(avg.steps.taken$avg_steps), ]
ast.interval[1]

## Imputing missing values
        ## Question 1
num_nas <- sum(is.na(data))
        ## Question 2 and Question 3
data2 <- data
for (i in 1:nrow(data2)) {
                ## if the data is shown to be an NA, it is replaced with the avg
        if(is.na(data2[i, 1])) {
                data2[i, 4] <- avg.steps.taken[which(data2$interval[i] == avg.steps.taken$interval),]$avg_steps} 
                ## if the data is not shown to be an NA, the original data was placed in
        else {data2[i, 4] <- data[i, 1]}
        colnames(data2)[4] <- "steps_rv"
}
                ## Rounding the data for the average to one decimal
data2$steps_rv <- round(data2$steps_rv,2)
                ## removing the old steps with the NAs
data2 <- data2[,c(4,2:3)]
                ## check to see how many NAs
sum(is.na(data2$steps_rv))
        ## Question 4
steps.taken.rv <- aggregate(data2$steps_rv, by = list(Date = data2$date), sum, na.rm = TRUE)
colnames(steps.taken.rv)[2] <- "num_steps"
steps_rv <- ggplot(steps.taken.rv, aes(Date, num_steps)) + 
        geom_bar(stat = "identity", colour = "green4", fill = "green4", width = 1) + 
        labs(title = "Total Number of Steps Taken by Day", 
             x = "Date", y = "Number of Steps by Taken per Day")
steps_rv
steps_rv.mean <- round(mean(steps.taken.rv$num_steps, na.rm = TRUE), 2)
steps_rv.median <- round(median(steps.taken.rv$num_steps, na.rm = TRUE), 2)


## Are there differences in activity between weekdys and weekends?
        ## Question 1
data2$dayofweek <- factor(format(data2$date, "%A"))
for (i in 1:nrow(data2)) {
        if (data2[i, 4] == "Saturday" | data2[i, 4] == "Sunday") {data2[i, 5] <- "Weekend"}
        else {data2[i, 5] <- "Weekday"}
        colnames(data2)[5] <- "Weekday_Weekend"
}
        ## Question 2
avg.steps.taken_rv <- aggregate(data2$steps_rv, 
                                by = list(Interval = as.numeric(data2$interval),Weekday_end = data2$Weekday_Weekend), 
                                mean, na.rm =TRUE)
colnames(avg.steps.taken_rv)[3] <- "avg_steps"
avg.steps.taken_rv$avg_steps <- round(avg.steps.taken_rv$avg_steps, 2)
library(lattice)
weekday_end <- xyplot(avg.steps.taken_rv$avg_steps ~ avg.steps.taken_rv$Interval | avg.steps.taken_rv$Weekday_end,
                      layout = c(1,2), type = "l", 
                      xlab = "Interval", ylab = "Average Number of Steps")
weekday_end
