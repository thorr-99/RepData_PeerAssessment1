---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity <- read.csv('activity.csv', na.strings = 'NA')
activity$date <- as.Date(activity$date, '%Y-%m-%d')
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?


```r
library(data.table)
activity <- data.table(activity)
activity.clean <- activity[complete.cases(activity),]
dailytotal <- activity.clean[, .(TotalSteps = sum(steps, na.rm = TRUE)), by=date]
cat('mean total number of steps taken per day:', mean(dailytotal$TotalSteps), '\n')
```

```
## mean total number of steps taken per day: 10766.19
```

```r
cat('median total number of steps taken per day:', median(dailytotal$TotalSteps))
```

```
## median total number of steps taken per day: 10765
```

```r
hist(dailytotal$TotalSteps, main='Histogram for Total Steps of each day', xlab = 'Total Steps for each day', col='forestgreen')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## What is the average daily activity pattern?


```r
avg.interval <- activity.clean[, .(Avg = mean(steps)), by=interval]

plot(avg.interval$interval, avg.interval$Avg, type='l', main='Averge Steps by Interval', 
     xlab = 'interval', ylab = 'Avg Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Find the interval which contains the maxium steps: 


```r
avg.interval[which(avg.interval$Avg == max(avg.interval$Avg)),]
```

```
##    interval      Avg
## 1:      835 206.1698
```


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
