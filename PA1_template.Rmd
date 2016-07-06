---
title: "Reproducible Research Course Project 1"
author: "Dominic Brown"
date: "July 6, 2016"
output: html_document
---

Loading and preprocessing the data

Show any code that is needed to:

1. Load the data.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
Activity <- read.csv("activity.csv", header=TRUE, sep =",", colClasses=c("numeric", "character", "numeric"), na.strings="NA")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis.

```{r}
Activity$date <- as.Date(Activity$date, format = "%Y-%m-%d")
Activity1 <- subset(Activity, !is.na(Activity$steps))
```

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day.

```{r}
TotalSteps <- tapply(Activity1$steps, Activity1$date, sum, na.rm=TRUE, simplify=T)
TotalSteps <- TotalSteps[!is.na(TotalSteps)]
```

2. Make a histogram of the total number of steps taken each day.

```{r, echo=TRUE}
hist(x=TotalSteps,
     col="red",
     breaks=20,
     xlab="Total Number of Steps per Day",
     ylab="Frequency",
     main="Histogram of the total number of steps per Day")

print(paste("Mean of the total number of steps taken per day: ", round(mean(TotalSteps),2)))
print(paste("Median of the total number of steps taken per day: ", median(TotalSteps)))
```

3. Calculate and report the mean and median of the total number of steps taken per day

Mean of the total number of steps taken per day:  10766.19

Median of the total number of steps taken per day:  10765

What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```{r}
IntervalAverage <- tapply(Activity1$steps, Activity1$interval, mean, na.rm=TRUE, simplify=T)
Activity3<- data.frame(interval=as.integer(names(IntervalAverage)), avg=IntervalAverage)
MaximumSteps <- max(Activity3$avg)
```

```{r, echo=TRUE}
with(Activity3,
     plot(interval,
          avg,
          col="red",
          type="l",
          xlab="5-minutes interval",
          ylab="Average Steps Across all Days",
          main="Aaverage Number of Steps"))

Activity3[Activity3$avg == MaximumSteps, ]
```         

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The 5-minute interval which had the maximum number of steps was the 835 interval. 

Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```{r}
sum(is.na(Activity$steps))
```
The total number of rows with steps = 'NA' is 2304.


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```{r}
Missing <- Activity
ndx <- is.na(Missing$steps)
IntervalAverage <- tapply(Activity1$steps, Activity1$interval, mean, na.rm=TRUE, simplify=T)
Missing$steps[ndx] <- IntervalAverage[as.character(Missing$interval[ndx])]
```


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
NewData <- tapply(Missing$steps, Missing$date, sum, na.rm=TRUE, simplify=T)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r, echo=TRUE}
hist(x=NewData,
     col="red",
     breaks=20,
     xlab="Total Number of Steps per Day",
     ylab="Frequency",
     main="Histogram of the total number of steps per Day)")

print(paste("Mean of the total number of steps taken per day: ", round(mean(NewData),2)))
print(paste("Median of the total number of steps taken per day: ", round(median(NewData),2)))
```
Mean of the total number of steps taken per day:  10766.19
Median of the total number of steps taken per day:  10766.19

Both results (mean & median) are now the same.

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
Weekday <- function(d) {
    wd <- weekdays(d)
    ifelse (wd == "Saturday" | wd == "Sunday", "weekend", "weekday")
}

wx <- sapply(Missing$date, Weekday)
Missing$wk <- as.factor(wx)
```

2. Make a panel plot containing a time series plot (i.e. type = "l) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r, echo=TRUE}
wk_All_Activities <- aggregate(steps ~ wk+interval, data=Missing, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(wk),
       layout = c(1, 2),
       col="blue",
       xlab="5-minute interval",
       ylab="Average Number of steps",
       type="l",
       lty=1,
       data=wk_All_Activities)
```


During weekdays most activities are in the morning, whereas the activities during weekend are distributed more evenly throughout the day.

