# Project 1

## Loading and preprocessing the data

Download the initial data into a directory named "data" in workspace.

```r
act <- read.csv("./data/activity.csv")
```

## What is mean total number of steps taken per day?

The histogram of the total number of steps taken each day is shown below.


```r
act_sum <- aggregate(act[,1] ~ act[,2], data=act, sum, na.rm=T)

hist(act_sum[,2], xlab="the total number of steps taken each day", main=" histogram of the total number of steps taken each day")
```

![plot of chunk question 1-1](figure/question 1-1-1.png) 

Mean of total number of steps taken per day is shown below.


```r
mean(act_sum[,2])
```

```
## [1] 10766.19
```

Median of total number of steps taken per day is shown below.


```r
median(act_sum[,2])
```

```
## [1] 10765
```

## What is the average daily activity pattern?

a time series plot is shown below.

```r
aves <- aggregate(act[,1] ~ act[,3], data=act, mean, na.rm=T)
plot(aves[,1], aves[,2], type = "l", xlab=" 5-minute interval", ylab="average number of steps taken")
```

![plot of chunk question 2-1](figure/question 2-1-1.png) 

The interval 104 contains the maximum number of steps.


```r
aves[which(aves[,2]==max(aves[,2])),]
```

```
##     act[, 3] act[, 1]
## 104      835 206.1698
```

## Imputing missing values

the total number of missing values in the dataset is 2304.


```r
sum(is.na(act[,1]))
```

```
## [1] 2304
```

A new dataset with missing value replaced by the average number of steps taken(averaged across all days).


```r
pos <- match(act[is.na(act[,1]),3], aves[,1])

modified_act <- act

modified_act[is.na(act[,1]),1] <- aves[pos,2]

modified_act_sum <- aggregate(modified_act[,1] ~ modified_act[,2], data=modified_act, sum, na.rm=T)

hist(modified_act_sum[,2], xlab="the total number of steps taken each day with missing values replaced", main=" histogram of the total number of steps taken \n each day with missing values replaced")
```

![plot of chunk question 3-2](figure/question 3-2-1.png) 

Mean of total number of steps taken per day with missing value replaced is shown below. The mean value is the same with/without the missing values replaced.



```r
mean(modified_act_sum[,2])
```

```
## [1] 10766.19
```

Median of total number of steps taken per day with missing value replaced is shown below. The median value become larger with the missing values replaced.



```r
median(modified_act_sum[,2])
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

The dataset with missing value replaced is divided into two datasets based on weekdays/weekends.


```r
library(plyr)
weekday <- weekdays(as.Date(modified_act[,2]))

day_factor <- mapvalues(weekday
                        , c("星期一", "星期二", "星期三", "星期四","星期五", "星期六","星期日")
                        , c(rep("weekday", 5), rep("weekend", 2)))

dayfactor_act <- cbind(modified_act, day_factor)

weekday_act <- dayfactor_act[which(dayfactor_act$day_factor=="weekday"),]

weekday_ave <- aggregate(weekday_act[,1] ~ weekday_act[,3], data=weekday_act, mean, na.rm=T)

weekend_act <- dayfactor_act[which(dayfactor_act$day_factor=="weekend"),]

weekend_ave <- aggregate(weekend_act[,1] ~ weekend_act[,3], data=weekend_act, mean, na.rm=T)
```

a time series plot below show the difference in activity patterns between weekdays and weekends. Higher activity frequency is observed in weekends.


```r
par(mfrow=c(2,1))

plot(weekday_ave[,1], weekday_ave[,2], type = "l", xlab="5-minute interval", ylab="average number of steps taken", main="activity patterns in weekdays")

plot(weekend_ave[,1], weekend_ave[,2], type = "l", xlab="5-minute interval", ylab="average number of steps taken", main="activity patterns in weekends")
```

![plot of chunk question 4-2](figure/question 4-2-1.png) 

