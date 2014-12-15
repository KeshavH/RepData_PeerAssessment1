---
title: "Peer Assessment 1"
---

##Loading and Preprocessing the Data:
###The data is unzipped and loaded, after being downloaded from Coursera

```r
unzip(zipfile="C:/Data_Science/repdata-data-activity.zip")
activityData<-read.csv("C:/Data_Science/activity.csv")
```

###The data is organized into daily steps, and  to answer later questions


```r
dailySteps <- aggregate(steps~date, data=activityData, sum, na.rm=TRUE)
dailySteps <- data.frame(dailySteps)

intervalMeanSteps <- aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
intervalMeanSteps <- data.frame(intervalMeanSteps)
```

##What is the mean total number of steps taken per day?
###A histogram of daily steps is created showing the frequency of each step count
###This is done with the ggplot2 package

```r
library(ggplot2)
qplot(x=steps,
      data=dailySteps,
      binwidth=max(dailySteps$steps)/5,
      main="Histogram of Daily Steps Taken",
      xlab="Daily Steps",
      ylab="Frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

###The mean amount of daily steps of 10766.19 and the median of 10765 are calculated through:

```r
mean(dailySteps$steps)
```

```
## [1] 10766.19
```
###and

```r
median(dailySteps$steps)
```

```
## [1] 10765
```
##What is the average daily activity pattern?
###Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days is created, again using ggplot2


```r
library(ggplot2)
ggplot(intervalMeanSteps, aes(x=interval, y=steps)) +
    geom_line() +
    ggtitle("Mean steps taken per interval") +
    xlab("Interval") +
    ylab("Mean steps taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
###Following this, we find the most active interval, *835*, to average *206.1698* steps though:

```r
intervalMeanSteps[which.max(intervalMeanSteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

##Inputing missing values
###Find the total number of missing values with:

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```
###All NA values in activityData$steps are replaced with the mean number of steps per interval it corresponds to.

```r
getMeanSteps <- function(interval) {
    intervalMeanSteps[which(intervalMeanSteps$interval==interval),]$steps
}
for (i in 1:length(activityData$steps)) {
    activityData$steps[i] <- ifelse(is.na(activityData$steps[i]),
                            getMeanSteps(activityData$interval[i]),
                            activityData$steps[i])
}
```
##Are there differences in activity patterns between weekdays and weekends?

###The data is split into weekdays and weekends, then plotted with ggplot2 to analyze trends

```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
activityData$date <- as.Date(activityData$date)
activityData$day <- sapply(activityData$date, FUN = weekday.or.weekend)

averages <- aggregate(steps ~ interval + day, data = activityData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 








