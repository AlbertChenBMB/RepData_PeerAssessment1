#unzip file
file<-unzip("activity.zip")
#read data
dataset<-read.csv("activity.csv")
head(dataset)
tail(dataset)
#What is mean total number of steps taken per day
#1. histogram of the total number of steps taken each day
library(dplyr)
library(ggplot2)

d_data <- group_by(dataset,date)
#calculate the total number of steps taken per day and plot hist
total_step_by_day<-summarise(d_data,total=sum(steps))
hist(total_step_by_day$total)

#calculate mean and median of step for each day
mean_step_by_day<-summarise(d_data,mean(steps,na.rm = TRUE))
median_step_by_day<-summarise(d_data,median(steps,na.rm = TRUE))

# What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
steps_by_interval<-aggregate(steps ~interval,d_data,mean)
plot(y=steps_by_interval$steps,x= steps_by_interval$interval,type = "l")

## 2. Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?
max_steps<-which.max(steps_by_interval$steps)
steps_by_interval[max_steps,]
# Imputing missing values
# Note that there are a number of days/intervals where there are missing values
# (coded as NA). The presence of missing days may introduce bias into some
# calculations or summaries of the data.
# 1. Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)
sum(is.na(dataset))

# 2. Devise a strategy for filling in all of the missing values in the dataset. The
# strategy does not need to be sophisticated. For example, you could use
# the mean/median for that day, or the mean for that 5-minute interval, etc.
new<-dataset
for(i in 1:nrow(new)){
        if(is.na(new$steps[i])){
                value<-new$interval[i] #find the na location
                s_value<-steps_by_interval[steps_by_interval$interval==value,]#find the interval value
                new$steps[i]<-s_value$steps#replace it
        }
}


# 3. Create a new dataset that is equal to the original dataset but with the
# missing data filled in.
no_na<-aggregate(steps~date,new,sum)
head(no_na)
# 4. Make a histogram of the total number of steps taken each day and Calculate
# and report the mean and median total number of steps taken per day.
hist(no_na$steps)
by_day_new<-group_by(new,date)
no_na_mean<-summarise(by_day_new,mean(steps))
no_na_median<-summarise(by_day_new,median(steps))

no_na_mean-mean_step_by_day
# Do these values differ from the estimates from the first part of the assignment?
no_na_mean$`mean(steps)`-mean_step_by_day$`mean(steps, na.rm = TRUE)`
no_na_median$`median(steps)`-median_step_by_day$`median(steps, na.rm = TRUE)`
# What is the impact of imputing missing data on the estimates of the total
# daily number of steps?

#Are there differences in activity patterns between weekdays and weekends?
new<-mutate(new, weekday=weekdays(as.Date(new$date)))
for(i in 1:nrow(new)){
        if(new[i,]$weekday == "土曜日"|new[i,]$weekday == "日曜日"){
                new[i,]$weekday<-"weekend"
        }else(new[i,]$weekday<-"weekday")
}        

new$weekday<-as.factor(new$weekday)
#calculate average steps by interval across all days
df_by_interval<-aggregate(steps ~ interval + weekday,new,sum)
