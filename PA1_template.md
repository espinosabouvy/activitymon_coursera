---
title: "Activity Monitoring"
author: "Luis Espinosa Bouvy"
date: "14 de octubre de 2016"
output:
  html_document: default
  html_notebook: default
---
#GENERAL REPORT FOR MONITORING ACTIVITIES

##Loading and cleaning data
Loading and cleaning data requires reading a csv containing the information  
from a zip file and giving some variables the correct format so as loading  
the required libraries.
```{r}
     library(ggplot2)
     library(dplyr)
     
     datos <- read.csv(unz("activity.zip", "activity.csv"))
     datos[,2] = as.Date(as.character(datos[,2]))
     datos <- datos
```
The total steps taken per day where:

```{r}

     by.day <- group_by(datos, date)%>%
          summarise("total" = sum(steps, na.rm=T))

     hist(by.day$total, col = "green", 
          main = "Daily Total Steps", xlab = "Total steps")

     mean(by.day$total)
     median(by.day$total)
     
```

The average daily activity pattern  for each 5 min interval is show in the  

```{r}

     por.interval <- group_by(datos, interval) %>%
          summarise("mean" = mean(steps, na.rm=T))
     media <- mean(por.interval$mean)

     qplot(interval, mean, data = por.interval, geom = "line",
           main = "Mean steps by interval of day") + 
          geom_hline(yintercept =  mean(por.interval$mean), 
                     col = "green")
```

The interval with the maximum mean steps is **`r por.interval[por.interval[,2] == max(por.interval[,2]),1]`**

The data includes NA values
```{r}

     sum(is.na(datos$steps))
```

The missing values were treated as follows:  
1. Found the mean steps for each interval 
2. Replaced NA values with mean steps of the interval

```{r}

     sum(is.na(datos$steps))

     por.interval <- group_by(datos, interval)%>%
          summarise("media" = round(mean(steps, na.rm=T), digits = 0))
     a <- merge(datos, por.interval, by = "interval")
     for.plot <- mutate(a, "fixed" = ifelse(is.na(a[,2]),media,steps))
     fixed.datos <- for.plot%>%
          select(interval, fixed, date)%>%
          rename("steps" = fixed)
     
     fixed.by.day <- group_by(fixed.datos, date)%>%
          summarise("total" = sum(steps, na.rm=T))

     by.day <- group_by(datos, date)%>%
          summarise("total" = sum(steps, na.rm=T))
```

Mean and median for original data are:
```{r}

     mean(fixed.by.day$total)
     median(fixed.by.day$total)
```

After computing NA's mean and median are:
```{r}

     mean(by.day$total)
     median(by.day$total)
```

Check the following plot to see the differences, the data is now more  
centralized, because of the strategy. 
```{r}

     by.day$origin <- "original"
     fixed.by.day$origin <- "fixed"
     for.plot <- rbind(by.day, fixed.by.day)

     ggplot(for.plot, aes(total, fill = origin)) + 
          geom_histogram(alpha = .5, position = 'identity',
                         binwidth = 5000) 
```

Making reference for the activities between weekday and weekends we can  
see no big differences

```{r}

     por.interval <- group_by(datos, interval)%>%
          summarise("media" = round(mean(steps, na.rm=T), digits = 0))
     a <- merge(datos, por.interval, by = "interval")
     for.plot <- mutate(a, "fixed" = ifelse(is.na(a[,2]),media,steps))
     fixed.datos <- for.plot%>%
          select(interval, fixed, date)%>%
          rename("steps" = fixed)
     fixed.datos$weekday <- as.POSIXlt(fixed.datos$date)$wday
     fixed.datos$type <- ifelse(fixed.datos$weekday > 5,"Weekend","Weekday")
     fixed.datos$type = as.factor(fixed.datos$type)
     

     for.plot <- fixed.datos%>%
          group_by(type, interval)%>%
          summarise("mean.steps" = mean(steps))
     

     ggplot(data = for.plot, aes(interval, mean.steps)) + 
          geom_line() + facet_grid(type~.) + geom_path(colour = "navy") +
          ylab("Mean step") + 
          ggtitle("Interval steps by weekday and weekend")
```
