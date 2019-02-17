---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

 Firstly, we read data and check it.

```r
dataset<-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

For this questioin, we group the data by day and store it as d_data, then plot the histogram of the total number of steps taken each day.  

We applied dplyr package to help us.

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
d_data <- group_by(dataset,date)
for_plot<-summarise(d_data,total=sum(steps))
hist(for_plot$total,main = "Histogram of steps taken each day",xlab = "sum of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
and we calculate the mean and median. 
The mean of steps taken each day is as follow table. 

```r
library(dplyr)
d_data%>% summarise(mean(steps,na.rm = TRUE))
```

```
## # A tibble: 61 x 2
##    date       `mean(steps, na.rm = TRUE)`
##    <fct>                            <dbl>
##  1 2012-10-01                     NaN    
##  2 2012-10-02                       0.438
##  3 2012-10-03                      39.4  
##  4 2012-10-04                      42.1  
##  5 2012-10-05                      46.2  
##  6 2012-10-06                      53.5  
##  7 2012-10-07                      38.2  
##  8 2012-10-08                     NaN    
##  9 2012-10-09                      44.5  
## 10 2012-10-10                      34.4  
## # ... with 51 more rows
```
The median of steps taken each day is as follow table. 

```r
library(dplyr)
d_data%>% summarise(median(steps,na.rm = TRUE))
```

```
## # A tibble: 61 x 2
##    date       `median(steps, na.rm = TRUE)`
##    <fct>                              <dbl>
##  1 2012-10-01                            NA
##  2 2012-10-02                             0
##  3 2012-10-03                             0
##  4 2012-10-04                             0
##  5 2012-10-05                             0
##  6 2012-10-06                             0
##  7 2012-10-07                             0
##  8 2012-10-08                            NA
##  9 2012-10-09                             0
## 10 2012-10-10                             0
## # ... with 51 more rows
```

## What is the average daily activity pattern?

We plot the 5-minumte interval and the average number of steps taken, averaged across all days.

```r
time_interval<- aggregate(steps ~interval,d_data,mean)
plot(time_interval$steps,type = "l",main = "Time interval",
     xlab = "5-minute interval",
     ylab = "Averaged Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
We also interested about the maximum number of steps across all the days in the dataset. 

```r
x<-time_interval[which.max(time_interval$steps),]$steps
```
The maximum number od steps is `x`.


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
