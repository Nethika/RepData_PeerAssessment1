###############################################
# Nethika Suraweera
# Coursera: Reproducible Research 
# Course Project 1
# 08/26/2017
# plot 1
###############################################

#rm(list = ls())
library(plyr)
library(ggplot2)
library(lattice) 

####### Download data #######
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile='data') 
unzip ("data", exdir = "./")

#Load data
activity_data = read.csv("activity.csv")

# Load data from repository:
#activity_data  <- read.csv(unz("activity.zip", "activity.csv"))

#preprocess
activity_data$date <- as.Date(activity_data$date)
clean_data <- activity_data[!is.na(activity_data$steps),]

#What is mean total number of steps taken per day?
total_steps <- aggregate(list(steps=clean_data$steps),by=list(date=clean_data$date), FUN=sum)
hist(total_steps$steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
#Save plot
dev.copy(png, file="hist_steps_per_day1.png", width=480, height=480)
dev.off()
median_steps <- median(total_steps$steps)
mean_steps <- mean(total_steps$steps)

#What is the average daily activity pattern?
intervals <- ddply(clean_data, .(interval), summarize, average = mean(steps))
plot( intervals$interval, intervals$average, type="l",main="Average Number of Steps per Interval",ylab="Average Number of Steps",xlab="Interval")
#Save plot
dev.copy(png, file="avg_steps_per_interval.png", width=480, height=480)
dev.off()
max_interval <- intervals$interval[which(intervals$average == max(intervals$average))]

#Imputing missing values
num_na <- sum(is.na(activity_data$steps))
# Strategry:  substitute the missing steps with the mean for that 5-minute interval.
activity_data2<- activity_data
activity_data2$steps[is.na(activity_data2$steps)] <- intervals[match(activity_data2$interval,intervals$interval),2]
total_steps2 <- aggregate(list(steps=activity_data2$steps),by=list(date=activity_data2$date), FUN=sum)
hist(total_steps2$steps, breaks=5, xlab="Steps", main = "Total Steps per Day")
#Save plot
dev.copy(png, file="hist_steps_per_day2.png", width=480, height=480)
dev.off()
median_steps2 <- median(total_steps2$steps)
mean_steps2 <- mean(total_steps2$steps)

#Are there differences in activity patterns between weekdays and weekends?

activity_data2$day_type<- ifelse(weekdays(activity_data2$date)%in% c('Saturday','Sunday') ,"weekend","weekday")
intervals2 <- ddply(activity_data2, .(interval, day_type), summarize, average = mean(steps))
xyplot(average~interval|day_type, data=intervals2 , type="l",  layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day", 
       ylab="Average Number of Steps", xlab="Interval")
#Save plot
dev.copy(png, file="steps_weekend_weekday.png", width=480, height=480)
dev.off()