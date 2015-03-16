---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Loading and preprocessing the data


```r
### Set the figure path to the path of the dataset
opts_chunk$set(fig.path='RepData_PeerAssessment1/figures/', fig.width=8, fig.height=6)

## Add my own library to the path. You may change this to your own library
.libPaths(c(.libPaths(),"./library")) 
library("dplyr") # load dplyr package
library("lubridate") # For manipulating date vector
library("ggplot2") # For plotting panel plot
ac_data <- read.csv("activity.csv",stringsAsFactors=F)
head(ac_data)
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

```r
actdata <- tbl_df(ac_data)
```

## What is total number of steps taken per day?
The total number of steps taken per day is:


```r
total_steps <- sapply(split(actdata$steps,actdata$date),sum,na.rm=T)
total_steps
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

### Histgram of the total number of steps taken per day.


```r
hist(total_steps,xlab="Total number of steps taken per day")
```

![plot of chunk histgram_1](RepData_PeerAssessment1/figures/histgram_1-1.png) 

### The mean and median of the total number of steps per day are:


```r
mean(total_steps)
```

```
## [1] 9354.23
```

```r
median(total_steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
fivemin_act <- group_by(actdata,interval)
fivemin_act <- summarise(fivemin_act,mean=mean(steps,na.rm=T))
plot(fivemin_act,type="l",ylab="Average steps taken across all days")
```

![plot of chunk daily_pattern](RepData_PeerAssessment1/figures/daily_pattern-1.png) 

It can be seen from the plot that the maxium number of steps appears at about 800 to 900 5-minute interval. In other words, maxium steps is at morning. The exact number is:


```r
max(fivemin_act$mean)
```

```
## [1] 206.1698
```

```r
filter(fivemin_act,mean==max(mean))
```

```
## Source: local data frame [1 x 2]
## 
##   interval     mean
## 1      835 206.1698
```
So 835 (That is 08:35 AM) 5-minute interval contains the maxium number of steps.

## Imputing missing values
### Total number of missing values

```r
## Calculate the total number of missing value
sum(is.na(actdata$steps)) 
```

```
## [1] 2304
```

There are 2304 missing values. 

### Filling the missing values
For simplicity, the missing values are all filled by 15.


```r
### A new dataset which is equal to the original one but with the missing
### values filled by 15.
newdata <- mutate(actdata,steps = replace(steps, is.na(steps), 15))
```

### The histgram of the total number of steps taken each day after filled NAs.


```r
new_total_steps <- group_by(newdata,date) %>% summarise(totalmean=sum(steps))    
hist(new_total_steps$totalmean,xlab="Total number of steps per day")
```

![plot of chunk new_histgram](RepData_PeerAssessment1/figures/new_histgram-1.png) 


The mean of the new dataset is:


```r
mean(new_total_steps$totalmean)
```

```
## [1] 9920.787
```

The median is:


```r
median(new_total_steps$totalmean)
```

```
## [1] 10395
```
When comparing with mean and median total number of steps taken per day above, the mean values differ from each other, however the median values are equal.
The imputing missing data process makes the total number of steps taken per day slightly different, so it will affect the average total number of steps taken per day and therefore will affect the mean total number of steps. 
The median value however is the middle one of a number list which the numbers are arranged in increasing order. If a number list has even numbers, then its median is the mean of two middle ones. That means, other numbers won't affect the median.
In this dataset, the median is 10395, which is the total number of steps taken on 2012-10-20. There are not missing values in each 5-minutes interval on 2012-10-20, imputing missing data won't change the value, so the median total number of steps taken per day doesn't change before and after imputing missing data.


```r
### Why median is not change
### Which day is the median total number of steps taken per day
total_steps[total_steps== 10395]
```

```
## 2012-10-20 
##      10395
```

```r
### Check if that day have missing values
any(is.na(select(filter(actdata,date=="2012-10-20"),steps)))
```

```
## [1] FALSE
```

## Are there differences in activity patterns between weekdays and weekends?

```r
### This function is used to judge whether a day is weekday or weekend.
judgeweekend <- function(x) {
    weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
    judge <- x %in% weekday
    if(judge) {
        x <- "weekday"
    }
    else {
        x <- "weekend"
    }
}
### Create a new data frame with new columns added and grouped by weekday
### and weekend,then summarised by average steps across all days in each 
### 5 min interval.
newdata1 <- newdata %>%
            mutate(weekdays = weekdays(ymd(date))) %>%
            mutate(weekdayorweekend = sapply(weekdays,judgeweekend)) %>%
            group_by(interval,weekdayorweekend) %>%
            summarise(meansteps = mean(steps))

### Make a plot
panelplot <- ggplot(
                newdata1, aes(interval,
                              meansteps,
                              group=weekdayorweekend)
                ) + 
                facet_wrap(~ weekdayorweekend,nrow=2) + 
                geom_line()
print(panelplot)
```

![plot of chunk difference_plot](RepData_PeerAssessment1/figures/difference_plot-1.png) 

The plot shows that there exists differences on average number of steps between weekday and weekend days. 
The number of steps taken between 0500 to 0900 in weekday days are higher than the numbers which are taken in weekend days. But weekend days have relatively higher number of steps between 1000 to 1600.
