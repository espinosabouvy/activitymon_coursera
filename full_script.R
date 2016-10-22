clean <- function(){
     #libraries
     library(ggplot2)
     library(dplyr)
     
     ##reading zip and data
     datos <- read.csv(unz("activity.zip", "activity.csv"))
     datos[,2] = as.Date(as.character(datos[,2]))
}

task1 <- function(){

     #steps by day
     by.day <- group_by(datos, date)%>%
          summarise("total" = sum(steps, na.rm=T))
     #histogram steps by day
     hist(by.day$total, col = "green", 
          main = "Daily Total Steps", xlab = "Total steps")
     #mean and median
     mean(by.day$total)
     median(by.day$total)
     
}
task2 <- function(){

     #steps by day
     by.interval <- group_by(datos, interval)%>%
          summarise("mean" = mean(steps, na.rm=T))
     media <- mean(by.interval$mean)
     #time series
     qplot(interval, mean, data = by.interval, geom = "line",
           main = "Mean steps by interval of day") + 
          geom_hline(yintercept =  mean(by.interval$mean), 
                     col = "green")
     #interval with mean max in day
     by.interval[by.interval[,2] == max(by.interval[,2]),]
     
}
task3 <- function(){
     
     #missing values
     sum(is.na(datos$steps))
     #fill the missing values by interval using average of same interval
     #of other days
     by.interval <- group_by(datos, interval)%>%
          summarise("media" = round(mean(steps, na.rm=T), digits = 0))
     a <- merge(datos, by.interval, by = "interval")
     for.plot <- mutate(a, "fixed" = ifelse(is.na(a[,2]),media,steps))
     fixed.datos <- for.plot%>%
          select(interval, fixed, date)%>%
          rename("steps" = fixed)
     
     ##fixed data without NA's
     fixed.by.day <- group_by(fixed.datos, date)%>%
          summarise("total" = sum(steps, na.rm=T))

     #original data
     by.day <- group_by(datos, date)%>%
          summarise("total" = sum(steps, na.rm=T))
     
     #mean and median
     mean(fixed.by.day$total)
     median(fixed.by.day$total)
     
     #mean and median
     mean(by.day$total)
     median(by.day$total)
     
     #table for plotting
     by.day$origin <- "original"
     fixed.by.day$origin <- "fixed"
     for.plot <- rbind(by.day, fixed.by.day)
     #histogram steps by day
     ggplot(for.plot, aes(total, fill = origin)) + 
          geom_histogram(alpha = .5, position = 'identity',
                         binwidth = 5000) 
     
}
task4 <- function(){
     
     #fill the missing values by interval using average of same interval
     #of other days
     by.interval <- group_by(datos, interval)%>%
          summarise("media" = round(mean(steps, na.rm=T), digits = 0))
     a <- merge(datos, by.interval, by = "interval")
     for.plot <- mutate(a, "fixed" = ifelse(is.na(a[,2]),media,steps))
     fixed.datos <- for.plot%>%
          select(interval, fixed, date)%>%
          rename("steps" = fixed)
     fixed.datos$weekday <- as.POSIXlt(fixed.datos$date)$wday
     fixed.datos$type <- ifelse(fixed.datos$weekday > 5,"Weekend","Weekday")
     fixed.datos$type = as.factor(fixed.datos$type)
     
     ##summary
     for.plot <- fixed.datos%>%
          group_by(type, interval)%>%
          summarise("mean.steps" = mean(steps))
     
     #plotting
     ggplot(data = for.plot, aes(interval, mean.steps)) + 
          geom_line() + facet_grid(type~.) + geom_path(colour = "navy") +
          ylab("Mean step") + 
          ggtitle("Interval steps by weeday and weekend")
          
}