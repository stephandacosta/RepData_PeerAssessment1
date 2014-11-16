# Reproducible Research: Peer Assessment 1

  
### Loading and preprocessing the data

unzip csv and read in data frame


```r
data <- read.csv(unz("activity.zip", "activity.csv"), header=TRUE ,sep=",")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


### What is mean total number of steps taken per day?

histogram of steps per day


```r
data_perday <- tapply(data$steps, data$date, sum, simplify=TRUE)
hist(data_perday, xlab="steps per day", main="")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


average steps per day


```r
mean(data_perday, na.rm=TRUE)
```

```
## [1] 10766.19
```

median steps per day


```r
median(data_perday, na.rm=TRUE)
```

```
## [1] 10765
```




### What is the average daily activity pattern?

plot of steps per interval


```r
tmp <- tapply(data$steps, data$interval, mean, na.rm=TRUE, simplify=TRUE)
steps_perinterval <- data.frame(interval=as.numeric(names(tmp)),avg_steps = tmp)
with(steps_perinterval, plot(interval,avg_steps, type="l", ylab="average steps in interval"))
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

interval with maximum of steps


```r
steps_perinterval[steps_perinterval[,"avg_steps"]==max(steps_perinterval$avg_steps),]$interval
```

```
## [1] 835
```


### Imputing missing values

number of missing values


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```


filling in NA with interval average


```r
filled_data <- merge(data,steps_perinterval,by="interval", all=TRUE)
filled_data$fill <- ifelse(is.na(filled_data$steps),filled_data$avg_steps, filled_data$steps)
head(filled_data)
```

```
##   interval steps       date avg_steps     fill
## 1        0    NA 2012-10-01  1.716981 1.716981
## 2        0     0 2012-11-23  1.716981 0.000000
## 3        0     0 2012-10-28  1.716981 0.000000
## 4        0     0 2012-11-06  1.716981 0.000000
## 5        0     0 2012-11-24  1.716981 0.000000
## 6        0     0 2012-11-15  1.716981 0.000000
```

recreate histogram of steps per day


```r
par(mfrow = c(1, 2))
data_perday <- tapply(data$steps, data$date, sum, simplify=TRUE)
hist(data_perday, main="steps per interval - excluding NA")
data_perday_filled <- tapply(filled_data$fill, filled_data$date, sum, simplify=TRUE)
hist(data_perday_filled, main="steps per day - filled NA")
```

![](./PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

the new average steps per day


```r
mean(data_perday_filled, na.rm=TRUE)
```

```
## [1] 10766.19
```

the new median steps per day


```r
median(data_perday_filled, na.rm=TRUE)
```

```
## [1] 10766.19
```

=> no signiifcant difference, the average has gone closer to the mean


### Are there differences in activity patterns between weekdays and weekends?


```r
filled_data$day_type <- ifelse(weekdays(as.Date(filled_data$date,"%Y-%m-%d"))=="Saturday" |weekdays(as.Date(filled_data$date,"%Y-%m-%d"))=="Sunday","weekend"," weekday")

make_plot <- function(dataframe){
tmp <- tapply(dataframe$steps, dataframe$interval, mean, na.rm=TRUE, simplify=TRUE)
steps_perinterval <- data.frame(interval=as.numeric(names(tmp)),avg_steps = tmp)
with(steps_perinterval, plot(interval,avg_steps, ylab="number of steps", type="l", main=dataframe$day_type[[1]]))
}

par(mfrow=c(2,1),  mar=c(2,2,2,2))
printnothing <- lapply(split(filled_data, filled_data$day_type), make_plot)
```

![](./PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

=> weekends are on average more active from the 1000 to 1800 intervals
