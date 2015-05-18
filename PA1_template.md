# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Following is the code to load the activity.csv dataset. The raw dataset is then filtered to remove rows with an NA value. Finally, date is converted from a chr class to a date class.


```r
library(data.table)
```

```
## Warning: package 'data.table' was built under R version 3.1.3
```

```
## 
## Attaching package: 'data.table'
## 
## The following object is masked _by_ '.GlobalEnv':
## 
##     .N
```

```r
# Load the csv file into a dataframe.  
chrFile = "C:\\coursera\\DataScientist\\5-ReproResearch\\Peer1\\RepData_PeerAssessment1\\data\\activity.csv"
dtAct <- data.table(read.csv(chrFile, header=TRUE, sep=',', na.strings="NA", 
                             check.names=FALSE, stringsAsFactors=FALSE, comment.char=""))

# Convert date from chr to date
dtAct$date <- as.Date(dtAct$date, format="%Y-%m-%d")
# Create a weekday factor
dtAct$weekday <- as.factor(weekdays(dtAct$date))
# Create a vector of day of the week numbers
dtAct$day = ifelse(wday(dtAct$date)==1,7,wday(dtAct$date)-1)
# How many observatation per weekday
#table(dtAct$weekday)
# How many observations per day
#table(dtAct$day)

# Put mean steps into dtAct
dfAvgStepsByInt <- as.data.frame(dtAct[, mean(steps, na.rm = TRUE),by = interval])
colnames(dfAvgStepsByInt)[2] <- "meansteps"
#str(dfAvgStepsByInt)
# Convert to data.table
dtAvgStepsByInt <- data.table(dfAvgStepsByInt)
str(dtAvgStepsByInt)
```

```
## Classes 'data.table' and 'data.frame':	288 obs. of  2 variables:
##  $ interval : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ meansteps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
# Set keys to do merge
setkey(dtAct, "interval")
setkey(dtAvgStepsByInt, "interval")
dtAct <- merge(dtAct, dtAvgStepsByInt)
```

## What is mean total number of steps taken per day?

Following is a histogram of the total number of steps take per day.


```r
library(ggplot2)

# Plot interval with day of week colored for each bar
p <- ggplot(dtAct, aes(x=day, y=steps, fill=factor(weekday))) + 
            stat_summary(fun.y="sum", geom="bar") +
            scale_x_discrete(breaks=c(1:7),
            labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) +
            ggtitle("Histogram of Steps per Day") + xlab("Day") + ylab("Steps") +
            scale_fill_discrete(name="Day of the Week", breaks = dtAct$weekday)

print(p)
```

```
## Warning in loop_apply(n, do.ply): Removed 2304 rows containing missing
## values (stat_summary).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Following is the mean and median of steps.


```r
mean(dtAct$mean, na.rm = TRUE)
```

```
## [1] 37.3826
```

```r
median(order(dtAct$mean), na.rm = TRUE)
```

```
## [1] 8784.5
```


## What is the average daily activity pattern?

Following is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
ggpMeanSteps = ggplot(dtAct, aes(interval, meansteps, type = "l")) + 
      geom_line() +
      labs(title="Mean Steps by Interval") + xlab("Interval") + ylab("Steps")

print(ggpMeanSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
dfAvgStepsByInt <- as.data.frame(dtAct[, mean(steps, na.rm = TRUE),by = interval])
colnames(dfAvgStepsByInt)[2] <- "mean"
mean(dfAvgStepsByInt$mean)
```

```
## [1] 37.3826
```

```r
dfMdnStepsByInt <- as.data.frame(dtAct[, median(order(steps), na.rm = TRUE),by = interval])
colnames(dfMdnStepsByInt)[2] <- "median"
median(order(dfMdnStepsByInt$median))
```

```
## [1] 144.5
```

The following shows which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps. The maximum number of steps occurs at interval 615


```r
summary(dtAct$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.00   37.38   12.00  806.00    2304
```

```r
dtAct[dtAct$steps == 806, ]
```

```
##    interval steps       date weekday day meansteps
## 1:      615   806 2012-11-27 Tuesday   2  63.45283
```

## Imputing missing values

Following are the total number of missing values in the dataset
(i.e. the total number of rows with NAs).


```r
# Which variables have NA
length(which(is.na(dtAct$steps)))
```

```
## [1] 2304
```

```r
length(which(is.na(dtAct$date)))
```

```
## [1] 0
```

```r
length(which(is.na(dtAct$interval)))
```

```
## [1] 0
```
As you can see there are 2304 missing step records. None of the records for date or interval are missing.

I have appended a meansteps to the dtAct data table. I will use the meansteps variable for those steps that have NA. I will calulate mean and median again. As you will see there is no difference in the means. However, the median is a more respecible value, Therefore, the subsitution policy works.


```r
# Recalc means
dtAct <- transform(dtAct, steps = ifelse(is.na(steps), as.integer(meansteps + 0.5), steps))

# Calc mean and median again.
dfAvgStepsByInt <- as.data.frame(dtAct[, mean(steps),by = interval])
colnames(dfAvgStepsByInt)[2] <- "mean"
mean(dfAvgStepsByInt$mean)
```

```
## [1] 37.38069
```

```r
dfMdnStepsByInt <- as.data.frame(dtAct[, median(order(steps)),by = interval])
colnames(dfMdnStepsByInt)[2] <- "median"
median(order(dfMdnStepsByInt$median))
```

```
## [1] 144.5
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
dtAct$weekdayend <- as.character(dtAct$weekday)

chrEndPtn = "Saturday|Sunday"
chrDayPtn = "Monday|Tuesday|Wednesday|Thursday|Friday"
dtAct$weekdayend <- gsub(chrEndPtn, "weekend", dtAct$weekdayend)
dtAct$weekdayend <- gsub(chrDayPtn, "weekday", dtAct$weekdayend)
dtAct$weekdayend <- as.factor(dtAct$weekdayend)

# Reorder columns for graphing
dtAct <- setcolorder(dtAct, c("day", "date", "weekday", "interval", "steps", "meansteps", "weekdayend" ))
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
# This seems to work, but it's filled in. Oh well, just forget about it
ppgWeekdayWeekend = ggplot(dtAct, aes(x = interval, y = steps, type = "l")) + 
      geom_line() +
      facet_grid(weekdayend ~ .) +
      labs(title="Mean Steps by Interval by Weekday or Weekend") + 
      xlab("Interval") + ylab("Steps")
print(ppgWeekdayWeekend)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

The following shows the mean of a weekday for each interval.


```r
require(sqldf)
```

```
## Loading required package: sqldf
```

```
## Warning: package 'sqldf' was built under R version 3.1.3
```

```
## Loading required package: gsubfn
```

```
## Warning: package 'gsubfn' was built under R version 3.1.3
```

```
## Loading required package: proto
## Loading required package: RSQLite
```

```
## Warning: package 'RSQLite' was built under R version 3.1.3
```

```
## Loading required package: DBI
```

```
## Warning: package 'DBI' was built under R version 3.1.3
```

```r
dtAct$weekdayend <- as.character(dtAct$weekdayend)
dtDay <- sqldf("select * from dtAct where weekdayend = 'weekday'")
```

```
## Loading required package: tcltk
```

```r
dfAvgStepsByIntDay <- as.data.frame(dtAct[, mean(steps, na.rm = TRUE), by = day])
mean(dtDay$steps)
```

```
## [1] 35.60864
```

The folllowing shows the mean of a weekend day.


```r
dtEnd <- sqldf("select * from dtAct where weekdayend = 'weekend'")
dfAvgStepsByIntEnd <- as.data.frame(dtAct[, mean(steps, na.rm = TRUE), by = day])
mean(dtEnd$steps)
```

```
## [1] 42.36458
```
As you can see weekday mean is 35.60864 and weekend mean is 42.36458.

So, it appears that the subject was more active during the weekend. Also, during the weekend most steps seem to occur by around interval 1500. Whereas, most step during the weekday seem to occur acount interval 1000. This indicates that the subject does most of his walking in short bursts during the weekday and most of his walking during the weekend in longer bursts of activity. 
