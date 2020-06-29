##Yutong Liu Course Project
=======================================

Read data

```r
activity<-read.csv("activity.csv")
```

Calculate the total numbers of steps taken per day

```r
steps_total<-aggregate(activity$steps, by=list(date=activity$date), FUN=sum)
print(steps_total)
```

```
##          date     x
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```

make histogram

```r
steps_total$x <- as.numeric(steps_total$x)
steps_total$date <- as.Date(steps_total$date,format="%Y-%m-%d")
steps_total$datetime <- paste(as.Date(steps_total$date))
steps_total$Datetime <- as.POSIXct(steps_total$datetime)
names(steps_total)[2] <- "steps"
library(ggplot2)
ggplot(steps_total,aes(x=date,y=steps))+geom_bar(stat="identity")
```

```
## Warning: Removed 8 rows containing missing values
## (position_stack).
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps_total$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_total$steps,na.rm=TRUE)
```

```
## [1] 10765
```
mean=10766.19
median=10765

Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
datetime <- strptime(paste(activity$date, activity$interval, sep=" "), "%Y-%m-%d %OS")
plot(datetime,activity$steps,type="l",na.rm=TRUE)
```

```
## Warning in plot.window(...): "na.rm"不是图形参数
```

```
## Warning in plot.xy(xy, type, ...): "na.rm"不是图形参数
```

```
## Warning in axis(side, at = z, labels = labels, ...):
## "na.rm"不是图形参数
```

```
## Warning in axis(side = side, at = at, labels =
## labels, ...): "na.rm"不是图形参数
```

```
## Warning in box(...): "na.rm"不是图形参数
```

```
## Warning in title(...): "na.rm"不是图形参数
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval_total<-aggregate(activity$steps, by=list(date=activity$interval), FUN=mean,na.rm=TRUE)
max(interval_total$x)
```

```
## [1] 206.1698
```

```r
which.max(interval_total$x)
```

```
## [1] 104
```
interval=835 has the maximum value

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
colSums(is.na(activity))
```

```
##    steps     date interval 
##     2304        0        0
```

```r
interval_mean<-aggregate(activity$steps, by=list(interval=activity$interval), FUN=mean,na.rm=TRUE)
imputed_activity <- activity
for (i in 1:nrow(imputed_activity)){
        if (is.na(imputed_activity[i,1]))
                for (j in 1:nrow(interval_mean))
                        if (imputed_activity[i,3] == interval_mean[j,1])
                                imputed_activity[i,1] = interval_mean[j,2]
}
imputed_activity$date <- as.Date(imputed_activity$date,format="%Y-%m-%d")
ggplot(imputed_activity,aes(x=date,y=steps))+geom_bar(stat="identity")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
imputed_activity[2]<-as.POSIXct(imputed_activity$date,format="%Y-%m-%d")
weekday<-imputed_activity[which(weekdays(as.Date(imputed_activity$date, format = "%Y-%m-%d"))
 %in% c('星期一','星期二', '星期三', '星期四', '星期五')), ]
weekend<-imputed_activity[which(weekdays(as.Date(imputed_activity$date, format = "%Y-%m-%d"))
 %in% c('星期六','星期日')), ]
weekday_datetime <- strptime(paste(weekday$date, weekday$interval, sep=" "), "%Y-%m-%d %M")
plot(weekday_datetime,weekday$steps,type="l",na.rm=TRUE)
```

```
## Warning in plot.window(...): "na.rm"不是图形参数
```

```
## Warning in plot.xy(xy, type, ...): "na.rm"不是图形参数
```

```
## Warning in axis(side, at = z, labels = labels, ...):
## "na.rm"不是图形参数
```

```
## Warning in axis(side = side, at = at, labels =
## labels, ...): "na.rm"不是图形参数
```

```
## Warning in box(...): "na.rm"不是图形参数
```

```
## Warning in title(...): "na.rm"不是图形参数
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
weekend_datetime <- strptime(paste(weekend$date, weekend$interval, sep=" "), "%Y-%m-%d %M")
plot(weekend_datetime,weekend$steps,type="l",na.rm=TRUE)
```

```
## Warning in plot.window(...): "na.rm"不是图形参数
```

```
## Warning in plot.xy(xy, type, ...): "na.rm"不是图形参数
```

```
## Warning in axis(side, at = z, labels = labels, ...):
## "na.rm"不是图形参数
```

```
## Warning in axis(side = side, at = at, labels =
## labels, ...): "na.rm"不是图形参数
```

```
## Warning in box(...): "na.rm"不是图形参数
```

```
## Warning in title(...): "na.rm"不是图形参数
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-2.png)
