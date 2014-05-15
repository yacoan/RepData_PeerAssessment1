setwd("C:/Users/js1/Dropbox/Repository/RepData_PeerAssessment1")

activity = read.csv("activity.csv", sep=",", na.strings = "NA")

activity$date <- as.Date(activity$date)
summary(activity)

library(ggplot2)
q<-qplot(date, weight=activity$steps, data=activity, geom="histogram")
print(q)

mean(tapply(activity$steps, activity$date, sum, na.rm = TRUE))
median(tapply(activity$steps, activity$date, sum, na.rm = TRUE))

average_steps<-data.frame(cbind(activity$interval,tapply(activity$steps, activity$interval, mean, na.rm = TRUE)))
colnames(average_steps) <- c("interval","steps")

q<-ggplot(data=average_steps,aes(x=interval,y=steps, color=steps)) +
    geom_line() 

print(q)

sum(is.na(activity))

average_steps <- average_steps[!duplicated(average_steps),]
nrow(average_steps)

fixed <- activity
    
for (i in 1:nrow(fixed)) {
    if (is.na(fixed[i, "steps"])){
        x <- fixed[i, "interval"]
        temp <- average_steps[average_steps$interval == x,]
        fixed[i,"steps"] <- temp$steps
        }
}

q<-qplot(date, weight=fixed$steps, data=fixed, geom="histogram")
print(q)

#################
?weekdays()
fixed

install.packages("timeDate")
library(timeDate)

for (i in 1:nrow(fixed)) {
    if (isWeekend(fixed[i, "date"])){
        #print(fixed[i, "date"])
        fixed[i, "wk"] <- "weekend"
    } else {
        fixed[i, "wk"] <- "weekday"
    }
}

fixed$wk <- as.factor(fixed$wk)


q<-ggplot(data=fixed,aes(x=interval,y=steps)) + facet_grid(wk ~ .) + geom_line()
print(q)

theme_get()