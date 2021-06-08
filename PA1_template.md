---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
unzip("./activity.zip")
activityData <- read.csv("./activity.csv")
summary(activityData)
names(activityData)
head(activityData)
## What is mean total number of steps taken per day?
stepsperday <- tapply(activityData$steps, activityData$date, sum)
View(stepsperday)
hist(stepsperday, xlab = "Number of Steps", main = "Histogram: Steps per Day")
meanperday <- mean(stepsperday, na.rm = TRUE)
meanperday
medianperday <- median(stepsperday, na.rm = TRUE)
medianperday
## What is the average daily activity pattern?
## Make a time series plot of the 5-minute interval and average number of steps taken, averaged across all days
StepsPerInterval <- tapply(activityData$steps, activityData$interval, mean, na.rm = TRUE)
plot(as.numeric(names(StepsPerInterval)), 
     StepsPerInterval, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Average Daily Activity Pattern", 
     type = "l")
## Which 5-Minute Interval, on average across all the days in the dataset contains the maximum number of steps
maxInterval <- names(sort(StepsPerInterval, decreasing = TRUE)[1])
maxInterval
maxSteps <- sort(StepsPerInterval, decreasing = TRUE)[1]
maxSteps

## Imputing missing values
## Calculate and report the total number of missing values in the data set
missingvals <- sum(is.na(activityData$steps))
missingvals
## Devise a strategy for filling in all of the missing values in the dataset 
## Create a new dataset that is equal to the original dataset but eith the missing data filled in 
StepsPerInterval <- tapply(activityData$steps, activityData$interval, mean, na.rm = TRUE)
# split activity data by interval
activity.split <- split(activityData, activityData$interval)
# fill in missing data for each interval
for(i in 1:length(activity.split)){
  activity.split[[i]]$steps[is.na(activity.split[[i]]$steps)] <- StepsPerInterval[i]
}
activity.imputed <- do.call("rbind", activity.split)
activity.imputed <- activity.imputed[order(activity.imputed$date) ,]
##Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day
StepsPerDay.imputed <- tapply(activity.imputed$steps, activity.imputed$date, sum)
hist(StepsPerDay.imputed, xlab = "Number of Steps", main = "Histogram: Steps per Day (Imputed data)")
MeanPerDay.imputed <- mean(StepsPerDay.imputed, na.rm = TRUE)
MedianPerDay.imputed <- median(StepsPerDay.imputed, na.rm = TRUE)
## Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels "weekday' and "weekend" indicating whether a diven date is a weekday or weekend day
activity.imputed$day <- ifelse(weekdays(as.Date(activity.imputed$date)) == "Saturday" | weekdays(as.Date(activity.imputed$date)) == "Sunday", "weekend", "weekday")
## Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekdays or weekend days
# Calculate average steps per interval for weekends
StepsPerInterval.weekend <- tapply(activity.imputed[activity.imputed$day == "weekend" ,]$steps, activity.imputed[activity.imputed$day == "weekend" ,]$interval, mean, na.rm = TRUE)

# Calculate average steps per interval for weekdays
StepsPerInterval.weekday <- tapply(activity.imputed[activity.imputed$day == "weekday" ,]$steps, activity.imputed[activity.imputed$day == "weekday" ,]$interval, mean, na.rm = TRUE)

# Set a 2 panel plot
par(mfrow=c(1,2))

# Plot weekday activity
plot(as.numeric(names(StepsPerInterval.weekday)), 
     StepsPerInterval.weekday, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Activity Pattern (Weekdays)", 
     type = "l")

# Plot weekend activity
plot(as.numeric(names(StepsPerInterval.weekend)), 
     StepsPerInterval.weekend, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Activity Pattern (Weekends)", 
     type = "l")
