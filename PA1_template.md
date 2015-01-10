# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)

zipFileName = "./activity.zip"
unzippedFileName = "./activity.csv"
if(!file.exists(unzippedFileName))
{
  unzip(zipFileName)
}
data = read.csv(unzippedFileName, stringsAsFactors = FALSE)
data = transform(data, date = as.Date(date))

options(scipen = 1, digits = 2)
```

## What is mean total number of steps taken per day?

```r
byDate <- summarise(
  group_by(data, date),
  steps = sum(steps, na.rm = TRUE)
)

hist(byDate$steps, main = "Number of steps per day", xlab = "Number of steps per day", ylab = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meanSteps = mean(byDate$steps)
medianSteps = median(byDate$steps)
```
The mean number of steps per day is 9354.23.  
The median number of steps per day is 10395.  


## What is the average daily activity pattern?

```r
byInterval <- summarise(
  group_by(data, interval),
  steps = mean(steps, na.rm = TRUE)
)

plot(byInterval$interval, byInterval$steps, type="l", xlab= "Interval", ylab= "Mean Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxIntervalIndex = which.max(byInterval$steps)
maxInterval = byInterval[maxIntervalIndex, 'interval']
maxIntervalSteps = byInterval[maxIntervalIndex, 'steps']
```
Interval 835 contains the maximum average number of steps across all days in the dataset (206.17).


## Imputing missing values

```r
numNA = sum(is.na(data$steps))

meanStepsPerInterval = summarise(
  group_by(data, interval),
  steps = mean(steps, na.rm = TRUE)
)

dataWithImputation = data
for (i in 1:nrow(data) ) {
  if(is.na(data[i,"steps"]))
  {
    interval = data[i, "interval"]
    dataWithImputation[i,"steps"] = meanStepsPerInterval[meanStepsPerInterval$interval == interval,"steps"]
  }
}

byDateWithImputation <- summarise(
  group_by(dataWithImputation, date),
  steps = sum(steps, na.rm = TRUE)
)

hist(byDateWithImputation$steps, main = "Number of steps per day with imputation", xlab = "Number of steps per day", ylab = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
meanStepsWithImputation = mean(byDateWithImputation$steps)
medianStepsWithImputation = median(byDateWithImputation$steps)
```
There are 2304 missing values in the data set.

If we impute the number of steps for an interval on a given day by the mean number of steps for that interval across all days:  
The mean number of steps per day is 10766.19.  
The median number of steps per day is 10766.19.

Note that imputation raised the estimated mean and median significantly.


## Are there differences in activity patterns between weekdays and weekends?


```r
dataWithImputation$dayType = factor(weekdays(dataWithImputation$date) %in% c("Saturday", "Sunday"), labels = c("weekday", "weekend"))
byIntervalAndDayType <- summarise(
  group_by(dataWithImputation, interval, dayType),
  steps = mean(steps, na.rm = TRUE)
)
xyplot(steps ~ interval | dayType, data = byIntervalAndDayType, type = "l", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

