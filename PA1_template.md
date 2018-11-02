Course project1
=======================================
```{r setoptions, cache=TRUE}
```
## Loading and preprocessing the data
```{r}
library(data.table)
library(ggplot2)
Sys.setlocale("LC_TIME","English")
if(!file.exists("D:\\R\\Rfile\\Reproducible research\\data.zip"))
{download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" , destfile = "D:\\R\\RfileReproducible research\\data.zip")}
if(!file.exists("D:\\R\\Rfile\\Reproducible research\\data.csv"))
{unzip("D:\\R\\Rfile\\Reproducible research\\data.zip", exdir = "D:\\R\\Rfile\\Reproducible research")}
```

## Reading csv Data into Data.Table
```{r}
activityDT <- fread("D:\\R\\Rfile\\Reproducible research\\activity.csv", data.table = F)
```

## What is mean total number of steps taken per day?
1.calculate the total number of steps taken per day
```{r}
df <- aggregate(steps ~ date, data = activityDT, sum)
head(df)
```
2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.
```{r}
g <- ggplot(df, aes(steps))
g + geom_histogram(fill = "mistyrose3", binwidth = 1000) + labs(x = "Steps per Day", y = "Occurances")
```

3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
median(df$steps)
mean(df$steps)
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. ???????????????? = "????") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
df_interval <- aggregate(steps ~ interval, data = activityDT, mean)
g <- ggplot(df_interval, aes(interval,steps))
g + geom_line(color="blue") + labs(y = "Avg. Steps per day", title = "Avg. Daily Steps")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
df_interval$interval[which.max(df_interval$steps)]
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)
```{r}
sum(is.na(activityDT))
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r}
# Filling in missing values with the mean for that 5-minute interval
missing_index <- is.na(activityDT)
missing_interval <- activityDT$interval[missing_index]
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
activityFilled <- transform(activityDT, steps = ifelse(is.na(activityDT$steps), df_interval$steps[match(missing_interval,df_interval$interval)] ,activityDT$steps ))
head(activityFilled)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
df_sum <- aggregate(steps ~ date, data = activityFilled, sum)
g <- ggplot(df_sum, aes(steps))
median(df_sum$steps)
mean(df_sum$steps)
g + geom_histogram(fill = "mistyrose3", binwidth = 1000) + labs(x = "Steps per Day", y = "Occurances") + labs(title = "Total Number of Steps Each Day (missing values replaced")
```

Type of estimate | mean | median
--- | --- | ---
First(NA not under consideration) | 10766.19 | 10765
Second(NA replaced) | 10766.19 | 10766.19

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
Myfunction<-  function(x){
        if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday"){
                x <- "weekend"
        }
        else 
                x <- "weekday"
}
activityDT$date <- as.Date(activityDT$date)
activityDT$week <- factor(sapply(activityDT$date, Myfunction))
head(activityDT)
```

2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r,echo=FALSE}
df_week <- aggregate(steps ~ interval + week, data = activityDT, mean) 
g <- ggplot(df_week, aes(interval, steps, color = week))
g + geom_line()  + facet_wrap(~week, ncol = 1, nrow=2) +labs(title = "Average Number of Steps taken (Weekdays vs. Weekends)", y = "No. of Steps")
```
 


