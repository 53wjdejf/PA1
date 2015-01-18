---
title: "Statistical Inference Peer Assessment 1"
date: "Thursday, January 15, 2015"
output: html_document
---

##I. Loading and preprocessing the data

1. Load the data

```r
a <- read.csv("Activity.csv")
```

2. Process the data type

```r
a$date <- strptime(a$date, "%Y-%m-%d")
```

##II. What is mean total number of steps taken per day?

  (Ignoring the missing values)

```r
a2 <- a[is.na(a$steps) == FALSE,]
```

1. Make a histogram of the total number of steps taken each day

```r
total_steps <- data.frame()
for (i in 1:(length(a$date)/288)-1){
  daily_steps <- data.frame(sum(a[(288*i+1):(288*i+288), 1]), a[288*i+1, 2])
  colnames(daily_steps) <- c("Sum_steps", "Date")
  total_steps <- rbind(total_steps, daily_steps)
}
processed_steps <- total_steps[!is.na(total_steps$Sum_steps),]
with(processed_steps, hist(Sum_steps, breaks=100, col="blue"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
  
2. Calculate and report the mean and median total number of steps taken per day

```r
mean(processed_steps[,1])
```

```
## [1] 10766.19
```

```r
median(processed_steps[,1])
```

```
## [1] 10765
```

##III. What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
a_allday <- data.frame()
for (i in 1:288){
  a_5minute <- data.frame(mean(a[a[i, 3] == a$interval,]$steps, na.rm=TRUE), i)
  colnames(a_5minute) <- c("Mean_steps", "Interval")
  a_allday <- rbind(a_allday, a_5minute)
}
plot(a_allday$Interval, a_allday$Mean_steps, xlab = "Interval", ylab = "Average number of steps", type = "l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
  (Interval data is processed to decimal system)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
a_allday <- data.frame()
for (i in 1:288){
  a_5minute <- data.frame(mean(a[a[i, 3] == a$interval,]$steps, na.rm=TRUE), i)
  colnames(a_5minute) <- c("Mean_steps", "Interval")
  a_allday <- rbind(a_allday, a_5minute)
  if (max(a_allday[,1]) == a_5minute[1,1]){
    a_maxsteps <- a_5minute
  }
}
print(a_maxsteps)
```

```
##   Mean_steps Interval
## 1   206.1698      104
```
  (Processed interval is "104". In raw data, it is "8:35 AM")

##IV. Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
a_missing <- a[is.na(a$steps) == TRUE | is.na(a$date) == TRUE | is.na(a$interval) == TRUE,]
nrow(a_missing)
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

  I'll use geometric mean of the two values(mean for that day, and mean for that 5-minute interval).
  by that, I can weigh on the smaller value much more. If geometric mean is also missing, 
  I'll use mean for that 5-minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
geom_mean <- function(a, b){
  (a*b)^(1/2)
}
```


```r
imputed <- a
for (i in 1:nrow(a)){
  if (is.na(a[i,1]) == TRUE){
  imputed[i,1] <- geom_mean(a_allday[(i %% 288)+1,1], total_steps[as.integer(i/288)+1,1]/288)
  }
  if (is.na(imputed[i,1]) == TRUE){
  imputed[i,1] <- a_allday[(i %% 288)+1,1]
  }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputed_steps <- data.frame()
for (i in 1:(length(a$date)/288)-1){
  daily_steps <- data.frame(sum(imputed[(288*i+1):(288*i+288), 1]), imputed[288*i+1, 2])
  colnames(daily_steps) <- c("Sum_steps", "Date")
  imputed_steps <- rbind(imputed_steps, daily_steps)
}
with(imputed_steps, hist(Sum_steps, breaks=100, col="green", xlab = "Total number of steps", main = "Imputed - taken each day"))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
  Some data has been added, and I can see that in the histogram. 
  Especially, data near the mean of "total number of steps taken each day" has been collected.

##V. Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.


```r
imputed_week <- cbind(imputed, rep(1:288, 61), rep(c(rep("weekday", 5), rep("weekend", 2)), 2, 17568 ), 
                      log(imputed$steps))
colnames(imputed_week) <- c("steps", "date", "interval", "count", "week", "log_steps")
print(names(imputed_week))
```

```
## [1] "steps"     "date"      "interval"  "count"     "week"      "log_steps"
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
library(ggplot2)
g <- ggplot(imputed_week, aes(count, steps))
g + geom_line() + facet_grid(week ~ .) + labs(x = "Interval", y = "Number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
