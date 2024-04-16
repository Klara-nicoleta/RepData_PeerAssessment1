---
title: "Project 1"
author: "Klara"
date: "2024-04-16"
output: html_document
---



##Loading and preprocessing the data


```r
library(readr)
activity <- read_csv("activity.csv")
```

```
## Rows: 17568 Columns: 3
## ── Column specification ─────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

### 1. What is the mean total number of steps taken per day?


```r
steps_per_day <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

hist(steps_per_day$steps,
     xlab= "Steps", ylab="Frequency", main = "Steps per Day",
     col = "steelblue")
```

![plot of chunk unnamed-chunk-57](figure/unnamed-chunk-57-1.png)

```r
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)
```

### 2. What is the average daily activity pattern?


```r
avg_steps_per_interval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)

plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type = "l",
     main = "Average Daily Activity Pattern", xlab = "5-Minute Interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-58](figure/unnamed-chunk-58-1.png)


### 3. Imputing missing values

```r
#total nr of NA
total_NA <- sum(is.na(activity$steps))
total_NA
```

```
## [1] 2304
```

```r
filled_data <- merge(activity, avg_steps_per_interval, by = "interval", suffixes = c("", "_mean"))

hist(filled_data$steps,
     xlab= "Steps", ylab="Frequency", main = "Steps per Day",
     col = "steelblue")
```

![plot of chunk unnamed-chunk-59](figure/unnamed-chunk-59-1.png)

```r
mean_steps_2 <- mean(steps_per_day$steps)
median_steps_2 <- median(steps_per_day$steps)
```

The mean and median barely differ so the impact of imputing missing data on the estimates of the total daily number of steps is minimal. 

### 4. Are there differences in activity patterns between weekdays and weekends?


```r
filled_data$day_type <- weekdays(filled_data$date)

# Define function to classify day type
classify_day <- function(day) {
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
    return("weekday")
  } else {
    return("weekend")
  }
}

filled_data$day_type <- factor(sapply(filled_data$day_type, classify_day), levels = c("weekday", "weekend"))

library(ggplot2)
filled_data$interval <- factor(filled_data$interval)

p <- ggplot(filled_data, aes(x = interval, y = steps, group = day_type, color = day_type)) +
  geom_line() +
  labs(title = "Average Number of Steps Taken by Time of Day",
       x = "5-Minute Interval",
       y = "Average Number of Steps") +
  facet_wrap(~ day_type, ncol = 1)

p
```

```
## Warning: Removed 3 rows containing missing values or values outside the scale
## range (`geom_line()`).
```

![plot of chunk unnamed-chunk-60](figure/unnamed-chunk-60-1.png)
