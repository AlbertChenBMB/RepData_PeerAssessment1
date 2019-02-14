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
#calculate the total number of steps taken per day

total_step_by_day<-summarise(d_data,total=sum(steps))
#calculate mean and median of step for each day
mean_step_by_day<-summarise(d_data,mean(steps,na.rm = TRUE))
median_step_by_day<-summarise(d_data,median(steps,na.rm = TRUE))
#plot hist
hist(total_step_by_day$total)

