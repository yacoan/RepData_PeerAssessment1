setwd("C:/Users/js1/Dropbox/Repository/RepData_PeerAssessment1")

activity = read.csv("activity.csv", header = TRUE, sep=",", 
                 colClasses = "character", na.strings = "NA")

head(activity)
tail(activity)
names(activity)

activity$steps <- as.numeric(activity$steps)
activity$date2 <- as.Date(activity$date , "%Y-%m-%d")

str(activity)

sum <- aggregate(activity$steps,by=list(activity$date2),sum) 

sum$x <- as.numeric(sum$x)
hist(sum)
plot(sum$x,sum$date, type="l")

