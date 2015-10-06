## Loading and preprocessing the data

# setwd("/home/ben/R/ReproducibleResearch/CourseProject1")

# Load the data
activity <- as.data.frame(read.csv("activity.csv"))

## What is mean total number of steps taken per day?

# Calculate the total number of steps taken per day
# (totalsteps is an array of integers)
totalsteps <- by(activity$steps,activity$date,FUN=sum, 
                 na.rm=TRUE)

# Make a histogram of the total number of steps 
# taken each day
hist(totalsteps, 
     main="Total Steps Per Day",
     xlab="Steps per day")

# Calculate and report the mean of the 
# total number of steps taken per day
cat("Mean total steps taken per day: ",
    mean(totalsteps))

# Calculate and report the median of the 
# total number of steps taken per day
cat("Median total steps taken per day: ",
    median(totalsteps))



## What is the average daily activity pattern?

# Calculate the mean of steps taken per interval
# (intervals is an array of integers)
avgsteps <- as.numeric(by(activity$steps,activity$interval,
                FUN=mean, na.rm=TRUE))

# Get the list of interval numbers
intervalnumber <- as.numeric(unique(activity$interval))

# Create a data frame in which each interval number
# is paired with its average number of steps
df <- as.data.frame(avgsteps)
df <- cbind(df, intervalnumber)

# Make a time series plot of the 5-minute interval
# (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)
plot(avgsteps ~ intervalnumber, df, type="l",
        main="Average Steps Taken Per Interval",
        xlab="Interval",
        ylab="Average steps taken")

# Max value is intervals[104] - 835, 206.1698
# which.max(df$avgsteps)

# Print that result
cat("5-minute interval with maximum number of steps on average: \n interval",
    df$intervalnumber[which.max(df$avgsteps)], "\n which has an average of", 
    df$avgsteps[which.max(df$avgsteps)], "steps across all days.")



## Imputing missing values

# Calculate and report the total number of missing 
# values in the dataset (i.e. the total number of 
# rows with NAs)
#sum(is.na(activity))
cat(sum(is.na(activity)), 
    "rows in the data frame contain NAs.")

# Replace each NA with the mean number of steps
# for that 5-minute interval
activity2 <- activity
activity2$steps <- ave(activity2$steps,activity2$interval,FUN=function(x){
        mm <- mean(x,na.rm=TRUE)
        ifelse(is.na(x),mm,x)
})


# Calculate the total number of steps taken per day
# (totalsteps is an array of integers)
totalsteps2 <- by(activity2$steps,activity2$date,FUN=sum, 
                 na.rm=TRUE)

# Make a histogram of the total number of steps 
# taken each day
hist(totalsteps2, 
     main="Total Steps Per Day",
     xlab="Steps per day")

# Calculate and report the mean of the 
# total number of steps taken per day
cat("Mean total steps taken per day: ",
    mean(totalsteps2))

# Calculate and report the median of the 
# total number of steps taken per day
cat("Median total steps taken per day: ",
    median(totalsteps2))



## What is the average daily activity pattern?

# Calculate the mean of steps taken per interval
# (intervals is an array of integers)
avgsteps2 <- as.numeric(by(activity2$steps,activity2$interval,
                          FUN=mean, na.rm=TRUE))

# Get the list of interval numbers
intervalnumber2 <- as.numeric(unique(activity2$interval))

# Create a data frame in which each interval number
# is paired with its average number of steps
df2 <- as.data.frame(avgsteps2)
df2 <- cbind(df2, intervalnumber2)

# Make a time series plot of the 5-minute interval
# (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)
plot(avgsteps2 ~ intervalnumber2, df2, type="l",
     main="Average Steps Taken Per Interval",
     xlab="Interval",
     ylab="Average steps taken")

# Max value is intervals[104] - 835, 206.1698
# which.max(df$avgsteps)

# Print that result
cat("5-minute interval with maximum number of steps on average: \n interval",
    df2$intervalnumber2[which.max(df2$avgsteps2)], "\n which has an average of", 
    df2$avgsteps2[which.max(df2$avgsteps2)], "steps across all days.")



## Are there differences in activity patterns 
## between weekdays and weekends?

# Create a new factor variable in the dataset with 
# two levels – “weekday” and “weekend” indicating 
# whether a given date is a weekday or weekend day.

# Create a new data frame
activity3 <- activity2

# Add a column with numbered days of the week
activity3$daytype <- as.POSIXlt(activity3$date)$wday

# Replace days 6 and 0 (Sat & Sun) with "weekend"
activity3$daytype[activity3$daytype == 0 | activity3$daytype == 6] <- "weekend"

# Replace days days that aren't weekends with "weekday"
activity3$daytype[activity3$daytype != "weekend"] <- "weekday"

# Make a panel plot containing a time series plot 
# (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged 
# across all weekday days (y-axis).

# Make a data frame for just weekdays
weekdays <- activity3[activity3$daytype=="weekday",]

# Find the average number of steps for each interval
# across all weekdays
avgsteps_wd <- as.numeric(by(weekdays$steps,weekdays$interval,
                           FUN=mean, na.rm=TRUE))

# Get the list of interval numbers
intervalnumber_wd <- as.numeric(unique(weekdays$interval))

# Create a data frame in which each interval number
# is paired with its average number of steps
df_wd <- as.data.frame(avgsteps_wd)
df_wd <- cbind(df_wd, intervalnumber_wd)

# Create a space for two side-by-side plots - 
# one for weekdays and one for weekends
days.par <- par(mfrow=c(1, 2))

# First make the plot for weekdays
plot(avgsteps_wd ~ intervalnumber_wd, df_wd, type="l",
     main="Average Steps Taken Per \n Interval on Weekdays",
     xlab="Interval",
     ylab="Average steps taken")

# AND add an equivalent plot for all weekend days

# Make a data frame for just weekends
weekends <- activity3[activity3$daytype=="weekend",]

# Find the average number of steps for each interval
# across all weekdays
avgsteps_we <- as.numeric(by(weekends$steps,weekends$interval,
                             FUN=mean, na.rm=TRUE))

# Get the list of interval numbers
intervalnumber_we <- as.numeric(unique(weekends$interval))

# Create a data frame in which each interval number
# is paired with its average number of steps
df_we <- as.data.frame(avgsteps_we)
df_we <- cbind(df_we, intervalnumber_we)

plot(avgsteps_we ~ intervalnumber_we, df_we, type="l",
     main="Average Steps Taken Per \n Interval on Weekends",
     xlab="Interval",
     ylab="Average steps taken")

#par(days.par)
