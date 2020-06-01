---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
*1. Load the data   
2. Process/transform the data (if necessary) into a format suitable for your analysis*


```r
activity <- read.csv(unz("activity.zip", "activity.csv"),header = TRUE)
activity$date <- as.Date(activity$date)
```

## What is mean total number of steps taken per day?
*1. Calculate the total number of steps taken per day*


```r
dailysteps <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
```

*2. Make a histogram of the total number of steps taken each day*

```r
hist(dailysteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

*3. Calculate and report the mean and median of the total number of steps taken per day*

```r
dailymean <- mean(dailysteps,na.rm=TRUE)
print(dailymean)
```

```
## [1] 9354.23
```

```r
dailymedian <- median(dailysteps,na.rm=TRUE)
print(dailymedian)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

*1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

```r
library(dplyr)
y <- group_by(activity,interval)
z <- summarize(y,mean=mean(steps,na.rm=TRUE))
with(z,plot(interval,mean,type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

*2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

```r
z[[which.max(z$mean),1]]
```

```
## [1] 835
```


## Imputing missing values

*1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*

```r
missing <- is.na(activity$steps)
sum(missing)
```

```
## [1] 2304
```

*2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*  
**For this problem, I have chosen to use the mean for the 5-minute interval. This has been calculated in the tbl z above.**

```r
actmerge <- merge(activity,z,all.x=TRUE)
actmerge <- actmerge[order(actmerge$date,actmerge$interval),]
rownames(actmerge)=NULL

filled <- actmerge
filled[is.na(filled$steps),"steps"] <- filled[is.na(filled$steps),"mean"]
filled <- filled[,-4]
```

*4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

```r
dailysteps2 <- tapply(filled$steps, filled$date, sum, na.rm=TRUE)
hist(dailysteps2)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
dailymean2 <- mean(dailysteps2,na.rm=TRUE)
print(dailymean2)
```

```
## [1] 10766.19
```

```r
dailymedian2 <- median(dailysteps2,na.rm=TRUE)
print(dailymedian2)
```

```
## [1] 10766.19
```
**When using the mean for the 5-minute interval as the fill-in value, the mean and median are higher and match each other**

## Are there differences in activity patterns between weekdays and weekends?

*1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*

```r
filled$dayofweek <- weekdays(filled$date)
weekkey <- data.frame("dayofweek" = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
                      "isweekday" = c("weekend","weekday","weekday","weekday","weekday","weekday","weekend"))
filledweek <- merge(filled,weekkey,all.x = TRUE)
filledweek <- filledweek[order(filledweek$date,filledweek$interval),]
rownames(filledweek)=NULL
filledweek <- filledweek[,-1]
```

*2. Make a panel plot containing a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*

```r
library(ggplot2)

yf <- group_by(filledweek,isweekday,interval)
zf <- summarize(yf,mean=mean(steps,na.rm=TRUE))
qplot(interval,mean,data=zf,geom="line",facets = isweekday~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
