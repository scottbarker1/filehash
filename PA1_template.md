---
title: "Reproducible Research Assignment 1"
author: "Scott Barker"
date: "March 5, 2016"
output: html_document
---
## Loading and preprocessing the data

##<<my-label, eval=TRUE, dev='png'>>=
  
1. Load the data(ie.. read.csv)  


```r
	if(file.exists("activity.csv")) {
  activity <- read.csv("activity.csv", colClass =c ('integer', 'Date', 'integer'))
}
```

2. Process/transform the data (if necessary) into a format suitable for your analysis
	

```r
  stepsPerday <- aggregate(steps ~ date, activity, sum)

    head(stepsPerday)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

```r
    stepsPerday
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

```r
    summary(stepsPerday)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```
    
## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
mean(stepsPerday$steps)
```

```
## [1] 10766.19
```

2. Make a histogram of the total number of steps taken each day

```r
barplot(stepsPerday$steps, names.arg=stepsPerday$date, ylim=c(0, 30000), xlab = "Date", ylab="sum(steps)", )
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(stepsPerday$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerday$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.interval <- aggregate(steps ~ interval, activity, mean)
plot(steps.interval, type='l')
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps.interval$interva[which.max(steps.interval$steps)]
```

```
## [1] 835
```

## Imputing Data 

1.Calculate and report the total number of missing values in the dataset (i.e. the

```r
missingval <- sum(is.na(activity$steps))
missingval 
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. 
Create a new dataset that is equal to the original dataset but with the missing data filled in.
	

```r
activity.clean <- merge(activity, stepsPerday, by="date", suffixes=c("", ".mean"))
  nas <- is.na(activity.clean$steps)
  activity.clean$steps[nas] <- activity.clean$steps.mean[nas]
  activity.clean <- activity.clean[, c(1:3)]
  head(activity.clean)
```

```
##         date steps interval
## 1 2012-10-02     0     1740
## 2 2012-10-02     0     1715
## 3 2012-10-02     0     1725
## 4 2012-10-02     0     1710
## 5 2012-10-02     0     1735
## 6 2012-10-02     0     1855
```

```r
  summary(activity.clean)
```

```
##       date                steps           interval     
##  Min.   :2012-10-02   Min.   :  0.00   Min.   :   0.0  
##  1st Qu.:2012-10-16   1st Qu.:  0.00   1st Qu.: 588.8  
##  Median :2012-10-29   Median :  0.00   Median :1177.5  
##  Mean   :2012-10-30   Mean   : 37.38   Mean   :1177.5  
##  3rd Qu.:2012-11-16   3rd Qu.: 12.00   3rd Qu.:1766.2  
##  Max.   :2012-11-29   Max.   :806.00   Max.   :2355.0
```

```r
  stepsclean <- aggregate(steps ~ date, activity.clean, sum)
  stepsclean
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

3.	Make a histogram of the total number of steps taken each day.


```r
stepsPerday <- aggregate(steps ~ date, activity.clean, sum)
barplot(stepsPerday$steps, names.arg=stepsPerday$date, ylim=c(0, 30000), 
        xlab="date", ylab="sum(steps)",)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

4. Calculate and report the mean and median total number of steps taken per day.


```r
mean(stepsPerday$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerday$steps)
```

```
## [1] 10765
```
No changes in the mean and median values of the data after the addition of the missing data because the steps were NA for the day.

## Are there differences in activity patterns between weekdays and weekends?

	1	Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
	

```r
dayType <- function(dates) {
  f <- function(date) {
    if (weekdays(date) %in% c("Saturday", "Sunday")) {
      "weekend"
    }
    else {
      "weekday"
    }
  }
  sapply(dates, f)
}
activity$dayType <- as.factor(dayType(activity$date))
str(activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ dayType : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

2. Make a panel plot containing a time series plot (i.e. type ="l") of the 5-minute interval (x-axis) and the average numbrer of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)

steps.interval <- aggregate(steps ~ interval + dayType, activity, mean)
xyplot(steps ~ interval | dayType, data=steps.interval, layout=c(2,1), type='l')
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

