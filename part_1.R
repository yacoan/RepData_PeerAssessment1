setwd("C:/Users/js1/Dropbox/Repository/RepData_PeerAssessment1")

activity = read.csv("activity.csv", header = TRUE, sep=",", 
                 colClasses = "character", na.strings = "NA")
names(activity)
nrow(activity)
activityT = activity[!is.na(activity$steps), ]
nrow(activityT)

activityT$steps <- as.numeric(activityT$steps)
activityT$date <- as.Date(activityT$date , "%Y-%m-%d")

sumStep = aggregate(steps~date, activityT, sum)


#What is mean total number of steps taken per day?
hist(sumStep$steps, col="red", main="Total number of steps taken each day", xlab="Steps Per Day")
mean(sumStep$steps,na.rm = T)
median(sumStep$steps,na.rm = T)

library(ggplot2)
q<-qplot(date, weight=activity$steps, data=activity, geom="histogram")
print(q)

#What is the average daily activity pattern?
#tips: type = "l"
names(activity)

avgStep = aggregate(steps~interval, activityT, mean)

plot(avgStep$interval,main='Average number of steps at every time interval',
     xlab='Time interval index', ylab='Steps', type='l')

which.max(avgStep$interval)