Loading and preprocessing the data
----------------------------------

1.  Load the data (i.e. read.csv())

2.  Process/transform the data (if necessary) into a format suitable for
    your analysis

``` r
library(readr)
activity <- read_csv("activity.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   steps = col_double(),
    ##   date = col_date(format = ""),
    ##   interval = col_double()
    ## )

``` r
#Remove the missing values
activity<-na.omit(activity)

# date formating for the date varible
activity$date<-as.Date(activity$date)

# view out the head of the data
head(activity)
```

    ## # A tibble: 6 x 3
    ##   steps date       interval
    ##   <dbl> <date>        <dbl>
    ## 1     0 2012-10-02        0
    ## 2     0 2012-10-02        5
    ## 3     0 2012-10-02       10
    ## 4     0 2012-10-02       15
    ## 5     0 2012-10-02       20
    ## 6     0 2012-10-02       25

What is mean total number of steps taken per day?
-------------------------------------------------

1.  Calculate the total number of steps taken per day

``` r
steps_per_day <- aggregate(steps ~ date, activity, sum)
```

2.If you do not understand the difference between a histogram and a
barplot, research the difference between them. Make a histogram of the
total number of steps taken each day

``` r
hist(steps_per_day$steps, main="Histogram of total number of steps per day", 
     xlab="Total number of steps in a day")
```

![](project1_files/figure-markdown_github/unnamed-chunk-3-1.png)
3.Calculate and report the mean and median of the total number of steps
taken per day

``` r
summary(steps_per_day$steps)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    8841   10765   10766   13294   21194

Mean of total number of steps per day is 10766, median is 10765.

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot (i.e. type = “l”) of the 5-minute interval
    (x-axis) and the average number of steps taken, averaged across all
    days (y-axis)

2.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

``` r
# Calculate average steps per interval for all days 
avg_steps_per_interval <- aggregate(steps ~ interval, activity, mean)

# Calculate average steps per day for all intervals - Not required, but for my own sake 
avg_steps_per_day <- aggregate(steps ~ date, activity, mean)

# Plot the time series with appropriate labels and heading
plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")
```

![](project1_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# Identify the interval index which has the highest average steps
interval_idx <- which.max(avg_steps_per_interval$steps)

# Identify the specific interval and the average steps for that interval
print (paste("The interval with the highest avg steps is ", avg_steps_per_interval[interval_idx, ]$interval, " and the no of steps for that interval is ", round(avg_steps_per_interval[interval_idx, ]$steps, digits = 1)))
```

    ## [1] "The interval with the highest avg steps is  835  and the no of steps for that interval is  206.2"

Imputing missing values
-----------------------

1.Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

1.  Devise a strategy for filling in all of the missing values in the
    dataset. The strategy does not need to be sophisticated. For
    example, you could use the mean/median for that day, or the mean for
    that 5-minute interval, etc.

2.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

3.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day. Do these values differ from the estimates from the first
    part of the assignment? What is the impact of imputing missing data
    on the estimates of the total daily number of steps?

``` r
library(readr)
activity <- read_csv("activity.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   steps = col_double(),
    ##   date = col_date(format = ""),
    ##   interval = col_double()
    ## )

``` r
missing_value_act <- activity[!complete.cases(activity), ]
nrow(missing_value_act)
```

    ## [1] 2304

note: We replaced the missing NA values with the average steps in that
interval across all the days.

``` r
# Substitute the NA values with the average steps in that interval across all the days

for (i in 1:nrow(activity)) {
    if(is.na(activity$steps[i])) {
        val <- avg_steps_per_interval$steps[which(avg_steps_per_interval$interval == activity$interval[i])]
        activity$steps[i] <- val 
    }
}

# Aggregate the steps per day with the imputed values
steps_per_day_impute <- aggregate(steps ~ date, activity, sum)

# Draw a histogram of the value 
hist(steps_per_day_impute$steps, main = "Histogram of total number of steps per day (IMPUTED)", xlab = "Steps per day")
```

![](project1_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
summary(steps_per_day_impute$steps)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10766   10766   12811   21194

Mean of total number of steps per day is 10766, median is 10766.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

2.  Make a panel plot containing a time series plot (i.e. type = “l”) of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).
    See the README file in the GitHub repository to see an example of
    what this plot should look like using simulated data.

``` r
#Cretae a function to determine if the date is a weekday
week_day <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}


# Apply the week_day function and add a new column to activity dataset
activity$day_type <- as.factor(sapply(activity$date, week_day))

#load the ggplot library
library(ggplot2)

# Create the aggregated data frame by intervals and day_type
steps_per_day_impute <- aggregate(steps ~ interval+day_type, activity, mean)

# Create the plot
plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("No of Steps")) +
    ggtitle("No of steps Per Interval by day type")
print(plt)
```

![](project1_files/figure-markdown_github/unnamed-chunk-10-1.png)

There is some subtle differences between the average number of steps in
weekdays and weekends. For instance, it appears that the user started a
bit later on weekend mornings and tend to do smaller numbers on weekend
mornings.
