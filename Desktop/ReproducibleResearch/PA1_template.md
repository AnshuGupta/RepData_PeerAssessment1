Reproducible Research: Peer Assessment 1
========================================================

### Loading and preprocessing the data

Load the required libraries for data analysis

```r
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.0.3
```

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.0.3
```


Firstly read data from the file "activity.csv" to a data frame DF. File with data should be in working directory.Process data as we need during the analysis.


```r
DF <- read.csv("activity.csv", header = TRUE, sep = ",")
```


## What is mean total number of steps taken per day?

We need sum up number of steps for each day using tapply function to steps colomn indexing by date colomn which is a factor variable.
We store results in daysteps array. Now we can make a histogram of the total number of steps taken each day. 


```r
daysteps <- tapply(DF$steps, DF$date, function(x) {
    sum(x, na.rm = TRUE)
})
```



```r
hist(daysteps, xlab = "Steps per day", main = "Histogram of steps per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


### Calculate and report the mean and median total number of steps taken per day

We use mean and median functions with na.rm = TRUE option applying to daysteps array.

```r
mean(daysteps, na.rm = TRUE)
```

```
## [1] 9354
```

```r
median(daysteps, na.rm = TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

Need to find average number of steps taken for each 5-minute interval.
Can be done by tapply function applying to steps column and indexing by interval column.
Inside of tapply function we use mean function with option na.rm = TRUE.

Store result in intsteps array and make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 


```r
intsteps <- tapply(DF$steps, DF$interval, function(x) {
    mean(x, na.rm = TRUE)
})
```



```r
plot(y = intsteps, x = as.numeric(names(intsteps)), type = "l", xlab = "Interval", 
    ylab = "Average number of steps", main = "Average number of steps per 5-minute interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


Find 5-minute interval that contains the maximum number of steps on average across all the days in the dataset.

```r
names(which(intsteps == max(intsteps, na.rm = TRUE)))
```

```
## [1] "835"
```


### Imputing missing values

Calculate and report the total number of missing values in the DF dataset (i.e. the total number of rows with NAs) using apply function for each column.

```r
totNA <- apply(DF, 2, function(x) {
    sum(is.na(x))
})
totNA
```

```
##    steps     date interval 
##     2304        0        0
```

totNA outputs the NA values in steps, date and interval column.

For filling the missing values in the DF dataset use the average mean value for that 5-minute interval. 
Create a new dataset newDF that is equal to the original dataset but with the missing data filled in.

```r
newDF <- DF
missing <- is.na(newDF$steps)
```


Using average mean number of steps per each 5-minutes interval.

```r
intsteps61 <- rep(intsteps, times = 61)
newDF$steps[missing] = intsteps61[missing]
```


Now we make a histogram of the total number of steps taken each day where all missing values were filled with average values.


```r
newdaysteps <- tapply(newDF$steps, newDF$date, function(x) {
    sum(x)
})
hist(newdaysteps, xlab = "Steps per day", main = "Histogram of steps per day")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 


calculate and report the mean and median of total number of steps taken per day using the new data with filled missing values. 

```r
mean(newdaysteps)
```

```
## [1] 10766
```

```r
median(newdaysteps)
```

```
## [1] 10766
```

The mean of total number of steps taken per day is 10766, the median is 10766.
These values differ from the previous estimates, now mean and median equal to each other.
They also have increased. The new mean is bigger the old one by 1411.9592, the new median is bigger by 371.1887.

## Are there differences in activity patterns between weekdays and weekends?

Now we create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Append this factor variable as column weekend to newDF data frame. 


```r
newDF$date <- as.Date(newDF$date)
newDF$weekend <- (weekdays(newDF$date) == "Saturday" | weekdays(newDF$date) == 
    "Sunday")
newDF$weekend <- factor(newDF$weekend, labels = c("weekday", "weekend"))
```


For plotting in lattice we need special data frame plotDF with numeric columns Interval, steps and factor column weekend.
Create temprorary df data frame, which is used to transform to plotDF data frame.


```r
df <- data.frame(tapply(newDF$steps, list(newDF$interval, newDF$weekend), mean))
df$interval <- as.numeric(rownames(df))

plotDF <- rbind(cbind(df$interval, df$weekend, rep(2, dim(df)[1])), cbind(df$interval, 
    df$weekday, rep(1, dim(df)[1])))

plotDF <- data.frame(plotDF)
names(plotDF) = c("Interval", "steps", "weekend")
plotDF$weekend = factor(plotDF$weekend, labels = c("weekday", "weekend"))
```


Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
xyplot(steps ~ Interval | weekend, data = plotDF, layout = c(1, 2), type = "l", 
    ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 


It is interesting that people walk more during the weekend, also they walk evenly during the weekend day.

```r
summary(df[c("weekend", "weekday")])
```

```
##     weekend          weekday      
##  Min.   :  0.00   Min.   :  0.00  
##  1st Qu.:  1.24   1st Qu.:  2.25  
##  Median : 32.34   Median : 25.80  
##  Mean   : 42.37   Mean   : 35.61  
##  3rd Qu.: 74.65   3rd Qu.: 50.85  
##  Max.   :166.64   Max.   :230.38
```


In the end clean computer memory removing all unnecessary data.

```r
rm(list = ls())
```

