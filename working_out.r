require(plyr)
require(sqldf)
require(ggplot2)



### Load and preprocess the data
# set working directory to where unzipped activity folder is

# Read the csv into memory
data <- read.csv("./activity/activity.csv")

# process the data into a suitable format for your analysis



### What is the mean total number of steps taken per day?

##1 Make a histogram of the total number of steps taken each day

# use the plyr package to summarise the data by date
dailyD <- ddply(data, "date", summarise,steps = sum(steps))

# create a histogram of steps per day
with(dailyD, hist(steps))

##2 calculate and report the mean and median totals of steps taken per day

# mean
dailyMean <- mean(dailyD[,"steps"], na.rm=T )

# median
dailyMedian <- median(dailyD[,"steps"], na.rm=T )


### What is the average daily pattern

##1 Make a time series plot of the 5 minute interval and the average number
## of steps taken, averaged across all days

# use the plyr package to summarise the data by interval
intervalD <- ddply(data, "interval", summarise,steps = mean(steps, na.rm=T))
# plot graph !!! add headers and stuff as well
with(intervalD, plot(interval,steps, type="l"))

##2 Which 5-minute interval, on average across all the days in the dataset,
## contains the maximum number of steps?
intervalD[intervalD[,"steps"]== max(intervalD[,"steps"]),"interval"]


### Imputing missing values

##1 Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)
sum(is.na(data[,"steps"]))

##2&3 Devise a strategy for filling in all of the missing values in the dataset.
## The strategy does not need to be sophisticated. For example, you could
## use the mean/median for that day, or the mean for that 5-minute interval, etc.


# use data from intervalD which contains the mean for each interval averaged by day

# copy data to new dataframe
filledData <- data
# find values with NA data
intList <- filledData[is.na(filledData[,"steps"]),]
# use sqldf to write the mean value of the interval per day over the NA values
filledData[is.na(filledData[,"steps"]),] <- sqldf("select intervalD.steps, intList.date, intList.interval from intList JOIN intervalD using(interval)")

##4 Make a histogram of the total number of steps taken each day and Calculate
## and report the mean and median total number of steps taken per day.
## Do these values differ from the estimates from the first part of the assignment?
## What is the impact of imputing missing data on the estimates of the total daily
## number of steps?

# use the plyr package to summarise the data by date
filledDailyD <- ddply(filledData, "date", summarise,steps = sum(steps))

# create a histogram of steps per day
with(filledDailyD, hist(steps, main()))

## calculate and report the mean and median totals of steps taken per day

# mean
filledDailyDMean <- mean(filledDailyD[,"steps"])

# median
filledDailyDMedian <- median(filledDailyD[,"steps"])


### Are there differences in activity patterns between weekdays and weekends?


# use filledData
# convert date from factor to date field
filledData[,"date"] <- as.Date(filledData[,"date"], format= "%Y-%m-%d")


# Function to return if the day is on the weekend or weekday
isWeekend <- function(day)
{
    # set initial value to weekday
    retrn <- "Weekday"
   
    # if the day is Saturday or Sunday change retrn to weekend
    if(day == "Saturday" | day == "Sunday") 
    {
        retrn <- "Weekend"
    }
    # return the value of retrn
    retrn    
}

# use weekdays to find the day of the week of each element of date
# use sapply to send this value to isWeekend
# add the returned value of isWeekend to a new column called timeofweek
filledData[,"timeofweek"] <- as.vector(sapply(weekdays(filledData[,"date"]), isWeekend))

# convert timeofweek to a factor variable
filledData[,"timeofweek"] <- as.factor(filledData[,"timeofweek"])

# use plyr to summarise the filled data by interval and timeofweek
filledIntervalD <- ddply(test, .(interval, timeofweek), summarise, steps=mean(steps))

# plot the weekday and weekend graphs
sp <- ggplot(data=t, aes(x=interval, y=steps, group=timeofweek, colour=timeofweek))
sp <- sp + facet_grid(timeofweek ~ .)
sp <- sp + geom_line() + geom_point()
sp












