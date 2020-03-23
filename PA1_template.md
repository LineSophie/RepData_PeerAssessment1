---
title: 'Reproducible Research: Programming Assignment'
author: "Line Ettrich"
date: "3/22/2020"
output: 
  html_document: 
    keep_md: yes
    self_contained: no
    smart: no
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

1. Loading and Preprocessing the Data

1.1 Downloading and Unzipping Data 

```r
if(!file.exists("./Data5")){dir.create("./Data5")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./Data5/ActivityMonitoring.zip")
unzip(zipfile="./Data5/ActivityMonitoring.zip",exdir="./Data5")
```

1.2 Reading the File

```r
ActivityMonitoring <- read.csv("./Data5/activity.csv", header = TRUE)
```

1.3 Reformatting the Date variable and Omitting NA's to transform the data into a format suitable for the analysis

```r
ActivityMonitoring$date <- format(as.Date(ActivityMonitoring$date), "%d/%m/%y")
ActivityMonitoring2 <- na.omit(ActivityMonitoring)
```

2. Constructing a Histogram for the Total Number of Steps taken each Day

2.1 Reading in neccessary Packages (dplyr)

```r
library(dplyr)
```

2.2 Calculating the Total Number of Steps per Day 

```r
Stepsperday <- ActivityMonitoring2 %>% 
  group_by(date) %>% 
  summarize(TotalSteps=sum(steps))
```

2.3 Constructing the Histogram for the Total Number of Steps taken each Day 

```r
hist(Stepsperday$TotalSteps, 
     main = "Total number of Steps taken each Day", 
     xlab = "Total Steps", 
     ylab = "Count", 
     col = "pink",
     ylim = c(0, 35))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

3. Calculating the Mean and Median Number of Steps taken each Day

```r
MeanSteps <- mean(Stepsperday$TotalSteps)
MedianSteps<- median(Stepsperday$TotalSteps)
```

4. Constructing a Time Series Plot of the Average Number of Steps taken

4.1 Calculating the Average Steps per Day

```r
AverageSteps <- ActivityMonitoring2 %>%
  group_by(interval) %>%
  summarize(AveSteps=mean(steps))
```

4.2 Constructing the Time Series Plot

```r
plot(AverageSteps$interval, AverageSteps$AveSteps, 
     type="l",
     xlab="Interval",
     ylab="Average steps",
     main="Average steps taken during 5 minute interval",
     cex.main = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

5. Calculating the 5-minute Interval that, on average, contains the maximum Number of Steps

```r
max_interval <- AverageSteps[which.max(AverageSteps$AveSteps),1]
max_interval
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```

6. Creating a new Dataset where NA's are replaced with Averages 

6.1 Calculating the Total Number of NA's in the Dataset

```r
MissingValuesActivity <- is.na(ActivityMonitoring$steps)
sum(MissingValuesActivity)
```

```
## [1] 2304
```

6.2 Code that describes and shows the Strategy for imputing missing Data (replacing NA's with Averages)

```r
NewActivityMonitoring <- ActivityMonitoring

for (i in 1:nrow(NewActivityMonitoring)) {
  if (is.na(NewActivityMonitoring$steps[i])) {
    indexvalue <- which(NewActivityMonitoring$interval[i] == AverageSteps$interval)
    NewActivityMonitoring$steps[i] <- AverageSteps[indexvalue,]$AveSteps
  }
}
```

7. Constructing the Histogram of the Total Number of Steps taken each Day after Missing Values are imputed

```r
AverageStepNew <- NewActivityMonitoring %>% group_by(date) %>% summarize(AveSteps2= sum(steps))

hist(AverageStepNew$AveSteps2,
     ylab = "Count",
     xlab = "Average Steps per Day",
     main = "Histogram: Number of steps taken each day",
     col = "pink",
     ylim = c(0,40),
     xlim = c(0,28000))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

8. Constructing a Panel Plot comparing the Average Number of Steps taken per 5-minute Interval across Weekdays and Weekends

8.1 Calculating the Mean and Median for imputed Data

```r
mean(AverageStepNew$AveSteps2)
```

```
## [1] 10766.19
```

```r
median(AverageStepNew$AveSteps2)
```

```
## [1] 10766.19
```

8.2 Assigning Weekday and Weekend Labels 

```r
NewActivityMonitoring$date <- as.POSIXct(NewActivityMonitoring$date, format = "%d/%m/%y")
NewActivityMonitoring$Day <- weekdays(NewActivityMonitoring$date)
NewActivityMonitoring$DayType <- "weekday"
NewActivityMonitoring$DayType[NewActivityMonitoring$Day %in% c("Saturday", "Sunday")] <- "weekend"
```

8.3 Loading Lattice Package

```r
library(lattice)
```

8.4 Computing the Average Steps for Weekdays/Weekends

```r
StepsByInterval <- aggregate(steps ~ interval + DayType, NewActivityMonitoring, mean)
```

8.4 Creating a Time Series Plot for the Average Steps per day by Interval

```r
xyplot(StepsByInterval$steps ~ StepsByInterval$interval|StepsByInterval$DayType, 
       main="Average Steps per Day by Interval",
       xlab="Interval", 
       ylab="Steps",
       layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
