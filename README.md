###### ReproducibleResearch
#### Project 1
activity <- read.csv("activity.csv")
activity <- transform(activity, 
                      datetime = strptime( paste(date,formatC(interval,width=4,flag="0")), "%Y-%m-%d %H%M"),
                      timeofday = strptime( paste("1970-01-01",formatC(interval,width=4,flag="0")), "%Y-%m-%d %H%M"))
str(activity)


### Total, Mean, & Median of steps taken per day

library(ggplot2)
plotstepsperday <- function( x ) {
    stepsperday <- aggregate( steps ~ date, data=x, FUN=sum)
    p <- ggplot(stepsperday, aes(steps))+geom_histogram(binwidth=5000,fill="orange",alpha=0.5)+labs(y="Frequency",title="Frequency of total steps taken in a day")
    print(p)

    stepsperday 
}
stepsperday <- plotstepsperday( activity )
mean(stepsperday$steps)
median(stepsperday$steps)

### Average of daily activity

library(ggplot2)
library(scales)
plotavgstepspertime <- function( x ) {
    avgstepspertime <- aggregate( steps ~ timeofday , data=x, FUN=mean)
    p <- ggplot(avgstepspertime,aes(timeofday,steps))+geom_line(color="red")+ scale_x_datetime(labels=date_format("%H:%M %p"))+labs(title="Average steps taken per five minute interval")
    print(p)    
    avgstepspertime    
}
avgstepspertime <- plotavgstepspertime( activity )
avgstepspertime[which.max(avgstepspertime$steps),]

### Imputing and Comparing missing values

sum(is.na(activity$steps))
library(dplyr)
activity.filled <- inner_join(activity,avgstepspertime,by="timeofday")
missing.values <- is.na(activity.filled$steps.x)
activity.filled$steps.x[missing.values] <- activity.filled$steps.y[missing.values]
activity.filled <- transform(activity.filled,steps = steps.x,steps.x=NULL,jhmc6 msteps.y=NULL)
                             
stepsperday.filled <- plotstepsperday( activity.filled )
mean(stepsperday.filled$steps)
median(stepsperday.filled$steps)

### Weekdays and weekends activity differences

weekend <- with(activity.filled, 
                ifelse( weekdays(datetime) %in% c("Saturday","Sunday"),"weekend","weekday"))
activity.filled$weekend <- factor(weekend)
steps.bywknd <- aggregate( steps ~ weekend + timeofday, data=activity.filled,FUN = mean)
library(ggplot2)
library(scales)
p <- ggplot(steps.bywknd,aes(x=timeofday,y=steps))+geom_line( col= "purple")+facet_grid(weekend~.)+scale_x_datetime(labels=date_format("%H:%M %p"))+labs(title="Average steps taken per five minute interval")
p

