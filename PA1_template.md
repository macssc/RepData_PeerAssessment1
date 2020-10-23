---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
df <- read.csv(unz('activity.zip', 'activity.csv'))
noNA <- na.omit(df)
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

```r
# Remove NA
total <- aggregate(noNA$steps, by=list(noNA$date), FUN=sum)
names(total) <- c('date', 'steps')
total$date <- as.Date(as.character(total$date), '%Y-%m-%d')
```

The total number of steps per day are:

```r
total
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

In histogram form:

```r
hist(total$steps, main='Total number of steps per day', xlab='Steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The mean total number of steps per day is:

```r
mean(total$steps)
```

```
## [1] 10766.19
```

The median total number of steps per day is:

```r
median(total$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
# assuming we can still ignore NAs
averages <- aggregate(noNA$steps, by=list(noNA$interval), FUN=mean)
names(averages) <- c('interval', 'steps')
plot(averages, type='l', main='Average number of steps per interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The interval with the maximum average number of steps is:

```r
averages$interval[which.max(averages$steps)]
```

```
## [1] 835
```

## Inputing missing values
The number of rows with missing values is:

```r
sum(is.na(df))
```

```
## [1] 2304
```

Fill missing values with the average amount of steps in an interval

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
filled <- df %>% mutate_at(vars(steps), ~ifelse(is.na(.), mean(., na.rm=TRUE), .))
```

In histogram form:

```r
total <- aggregate(filled$steps, by=list(filled$date), FUN=sum)
names(total) <- c('date', 'steps')
hist(total$steps, main='Total number of steps per day', xlab='Steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The mean total number of steps per day is:

```r
mean(total$steps)
```

```
## [1] 10766.19
```

The median total number of steps per day is:

```r
median(total$steps)
```

```
## [1] 10766.19
```

We can see that the average and median is very similar but we can see that the frequency of days where between 10000 and 15000 steps were made increased.

## Are there differences in activity patterns between weekdays and weekends?


```r
# Add new factor
filled$date <- as.Date(as.character(filled$date), '%Y-%m-%d')
weekdaysStr <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
filled$day <- factor(weekdays(filled$date) %in% weekdaysStr, labels=c('weekend', 'weekday'))

week <- subset(filled, day == 'weekday')
avgWeek <- aggregate(week$steps, by=list(week$interval), FUN=mean)
names(avgWeek) <- c('interval', 'steps')

weekend <- subset(filled, day == 'weekend')
avgWeekend <- aggregate(weekend$steps, by=list(weekend$interval), FUN=mean)
names(avgWeekend) <- c('interval', 'steps')


par(mfrow=c(2,1))
plot(avgWeek, type='l', ylab='Number of steps')
plot(avgWeekend, type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

