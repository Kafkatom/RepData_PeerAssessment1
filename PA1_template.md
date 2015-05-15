---
title: "Reproducible Research"
author: "Tom Kafka"
date: "Friday, May 15, 2015"
output: html_document
---

We download the file from internet to our working directory by using library RCURL and option for downloading file from https site. We read the content of this zip file to variable data.


```r
library(RCurl)
setInternet2(use = TRUE)
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destURL <- "C:/Users/Conrad/Documents/repdata.zip"
file <- download.file(url= fileURL, destfile= destURL, method ="auto")
data <- read.csv(unzip("repdata.zip",file="activity.csv"))
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
Checking the summary of data. Since there are NA's in steps column, we choose the new dataset without them.


```r
data2 <- data[which(!is.na(data$steps)),]
```
Then we calculate the total number of steps taken per day

```r
stepsNum <- tapply(data2$steps, data2$date, sum)
```
This is the histogram of total number of steps each day:


```r
hist(stepsNum, main="Total numbers of steps taken each day", xlab="Steps", ylim=c(0,40))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Calculate mean and median of steps taken per day:
Since there are some days with NA's, we display mean and medium by summary of stepsNum variable.

```r
summary(stepsNum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```
If we remove NA's, we get this results:

```r
 mean(stepsNum, na.rm= TRUE)
```

```
## [1] 10766.19
```

```r
 median(stepsNum, na.rm=TRUE)
```

```
## [1] 10765
```
Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis):


```r
average <- tapply(data2$steps, data2$interval, mean)
plot(y=average, x= names(average), type = "l",
      main="Average number of steps per 5-minute interval through days",
     ylab="Average number of steps",
      xlab="Time of Day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

```r
average[average==max(average)]
```

```
##      835 
## 206.1698
```
Total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```
There are 2304 missing values in the original data. We will fill this NA's with mean for that 5 minute interval, because there are several days with missing values, so we could not fill it with mean or medium for that day. On the other hand, we want to connect this part of the assignment with previous task.

```r
newData <- data
newData[which(is.na(newData$steps)),1]<-average[as.character(newData[which(is.na(newData$steps)),3])]
```

Histogram of the total number of steps taken each day from the new dataset without NA's.

```r
newStepsNum <- tapply(newData$steps, newData$date, sum)
hist(newStepsNum, main="Total numbers of steps taken each day
     (missing values replaced by intervals' mean", xlab="Steps", ylim=c(0,30))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

Calculate mean and median total number of steps taken per day, and compare them with the estimates from the first part of the assignment to see the impact of imputing missing data on the estimates of the total daily number of steps.


```r
mean(newStepsNum)
```

```
## [1] 10766.19
```

```r
median(newStepsNum)
```

```
## [1] 10766.19
```

```r
 mean(stepsNum, na.rm= TRUE)
```

```
## [1] 10766.19
```

```r
 median(stepsNum, na.rm=TRUE)
```

```
## [1] 10765
```

```r
summary(newStepsNum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

```r
summary(stepsNum)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```
We can see the biggest difference between quantiles, minimal between medians and none between means of this two datasets. 


Using weekdays function on date variable, we create new variable named week, which we divide
on weekend and workday, and put into datasets new variable day.

Using weekdays function on date variable, we create new variable named week, which we divide
on weekend and workday, and put into datasets new variable day.


```r
week <- weekdays(as.POSIXlt(newData$date,format="%Y-%m-%d"))
week[which(week=="Sunday"| week=="Saturday")]<-"Weekend"
week[which(week != "Weekend")] <- "Workday"
newData$day <- as.factor(week)
```

Using aggregate, we calculate the mean of interval for weekends and workdas. We name the variables in the average set interval, day and steps.

```r
average <- aggregate(newData$steps, list(newData$interval,newData$day), mean)
names(average) = c("interval","day","steps")
```

Using library lattice, we produce xyplot showing difference between weekends and workdays. 


```r
library(lattice)

xyplot(average$steps ~ average$interval | average$day, type = "l",
     main="Difference between weekends and workdays",
     ylab="Average number of steps",
     xlab="Time of Day", 
     layout=c(1,2))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 

From the plot we can see, there is more activity in the morning of workdays (before 10:00AM), but it seems there is more activity from 10:00AM to 20:00PM on weekends.
