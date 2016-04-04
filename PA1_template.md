{\rtf1\ansi\ansicpg1252\cocoartf1265\cocoasubrtf210
{\fonttbl\f0\fnil\fcharset0 HelveticaNeue;\f1\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;\red38\green38\blue38;}
\paperw11900\paperh16840\margl1440\margr1440\vieww25400\viewh13540\viewkind0
\deftab720
\pard\pardeftab720\sl640\sa320

\f0\b\fs30 \cf2 Reproducible Research: Peer Assessment 1
\f1\fs24 \cf0 \
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural
\cf0 Basic settings\

\b0 \
echo = TRUE  # Always make code visible\
options(scipen = 1)  # Turn off scientific notations for numbers\

\b \
Loading and preprocessing the data\

\b0 \
if (!file.exists("activity.csv")) \{\
  unzip("activity.zip")\
\}\
activity <- read.csv("activity.csv", colClass=c('integer', 'Date', 'integer'))\
\

\b What is mean total number of steps taken per day?
\b0 \
1. The total number of steps taken per day.\
\
steps.date <- aggregate(steps ~ date, activity, sum)\
head(steps.date)\
\
2. Histogram of the total number of steps taken each day.\
\
barplot(steps.date$steps, names.arg=steps.date$date, ylim=c(0, 25000), \
        xlab="date", ylab="sum(steps)",)\
\
3.1. Mean of total number of steps taken per day\
\
mean(steps.date$steps)\
\
3.2. Median of total number of steps taken per day\
\
median(steps.date$steps)\
\

\b What is the average daily activity pattern?
\b0 \
1. Time series plot of the 5-minute interval and average number of steps taken averaged across all days\
\
steps.interval <- aggregate(steps ~ interval, activity, mean)\
plot(steps.interval, type='l')\
\
2.  The 5-minute interval contains the maximum number of steps\
\
steps.interval$interval[which.max(steps.interval$steps)]\
\

\b Imputing missing values
\b0 \
1. The total number of missing values in the dataset is\
\
sum(is.na(activity$steps))\
\
2. The strategy for filling in all of the missing values in the dataset is to use mean of the day. \
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.\
\
activity.clean <- merge(activity, steps.date, by="date", suffixes=c("", ".mean"))\
nas <- is.na(activity.clean$steps)\
activity.clean$steps[nas] <- activity.clean$steps.mean[nas]\
activity.clean <- activity.clean[, c(1:3)]\
head(activity.clean)\
\
4. Histogram of the total number of steps taken each day\
\
steps.date <- aggregate(steps ~ date, activity.clean, sum)\
barplot(steps.date$steps, names.arg=steps.date$date, ylim=c(0, 25000), \
        xlab="date", ylab="sum(steps)",)\
\
4.1. Mean of total number of steps taken per day\
\
mean(steps.date$steps)\
\
4.2. Median of total number of steps taken per day\
\
median(steps.date$steps)\
\
# Data don\'92t appear to be significant different because imputation uses mean for that particular day but steps are NA for that entire day.\
\

\b Are there differences in activity patterns between weekdays and weekends?
\b0 \
1. Add new factor variable dayType with 2 levels \'96 \'93weekday\'94 and \'93weekend\'94\
\
dayType <- function(dates) \{\
  f <- function(date) \{\
    if (weekdays(date) %in% c("Saturday", "Sunday")) \{\
      "weekend"\
    \}\
    else \{\
      "weekday"\
    \}\
  \}\
  sapply(dates, f)\
\}\
\
activity$dayType <- as.factor(dayType(activity$date))\
str(activity)\
\
2. Panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays and weekends\
\
library(lattice)\
\
steps.interval <- aggregate(steps ~ interval + dayType, activity, mean)\
xyplot(steps ~ interval | dayType, data=steps.interval, layout=c(2,1), type='l')\
}
