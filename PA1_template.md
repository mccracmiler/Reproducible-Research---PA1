Reproducible Research Peer Assesment Project 1
Write a report answering given questions
============================================================================================================
Question 1. What is mean total number of steps taken per day?  Ignore the missing values in the dataset

    1.	Calculate the total number of steps taken per day
    
    2.	Make a histogram of the total number of steps taken each day
    
    3.	Calculate and report the mean and median of the total number of steps taken per day
    
Question 2. What is the average daily activity pattern?

   1.	Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps 
       taken, averaged across all days (y-axis)
       
   2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
   
Question 3. Imputing missing values

   1.	Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
   2.	Devise a strategy for filling in all of the missing values in the dataset. 
   3.	Create a new dataset that is equal to the original dataset but with the missing data filled in.
   4.	Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
   5.  Do these values differ from the estimates from the first part of the assignment? 
   6.  What is the impact of imputing missing data on the estimates of the total daily number of steps?
   
Question 4. Are there differences in activity patterns between weekdays and weekends?

   1.	Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
   2.	Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, 
       averaged across all weekday days or weekend days (y-axis). 


=========================================================================================================

This chunk of code checks to see if all the packages needed to run this code are installed  
and it calls the libraries to make sure they are available
while this may be overkill it is critical to insure the environment is replicated
each call to packages checks to see if the called package is installed and installs it if it is not

=========================================================================================================
 

```r
packages<-function(x){    #function to detemine if needed packages are installed
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(knitr)         #install and load knitr
packages(dplyr)         #install and load dplyr
packages(ggplot2)       #install and load ggplot2
packages(gridExtra)     #install and load gridExtra
packages(grid)          #install and load grid
packages(mosaic)        #install and load mosaic 
packages(RGraphics)     #install and load RGraphics 
packages(downloader)    #install and load downloader 
packages(lubridate)
remove(packages)        #function no longer needed
```

=========================================================================================================

This step downloads the files needed.  It checks to see if the files are already
downloaded before downloading and unzipping


```r
if (!file.exists("activity.csv")) {   #if this file already exist it it will skip the download and unzipped step 
    fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download(fileurl, dest="dataset.zip", mode="wb")
    unzip ("dataset.zip", exdir = ".")  # xtracts the file activity.csv to the working directory 
}
```
=========================================================================================================

Code chunk 1 will read and clean the data


```r
filenam <- "./activity.csv"        
activitydata <- read.csv(filenam, header = TRUE, sep = ",", na.strings = "NA") 
activitydata$date <- as.Date(activitydata$date , "%Y-%m-%d")
```

==========================================================================================================

Question 1. What is mean total number of steps taken per day?  Ignore the missing values in the dataset

    1.	Calculate the total number of steps taken per day
    2.	Make a histogram of the total number of steps taken each day

==========================================================================================================


```r
totalstepsperday_wo_NA  <- activitydata %>%
    group_by(date) %>% 
    summarize(stepsperday = sum(steps, na.rm = TRUE)) #create steps per day 
  
g1<-ggplot(totalstepsperday_wo_NA, aes(stepsperday)) + geom_histogram(binwidth=500, fill=NA, color="blue") + 
        theme_bw() + ylab("Number of Days") + xlab("Steps per Day") 
grid.arrange(g1, nrow=1)  
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
meantotalstepsperday_wo_NA <- mean(totalstepsperday_wo_NA$stepsperday)
summary(totalstepsperday_wo_NA$stepsperday)   # this produces a summary of statistics related to steps per day including the mean 
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

===============================================================================================================

Question 2. What is the average daily activity pattern?

   1.	Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
   
   2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

===============================================================================================================


```r
avg_steps_per_interval_wo_NA  <- activitydata %>%
    group_by(interval) %>% 
    summarize(avg_steps_per_interval = mean(steps, na.rm = TRUE)) #create average steps per 5 minute interval 
   
barplot(avg_steps_per_interval_wo_NA$avg_steps_per_interval, names.arg = avg_steps_per_interval_wo_NA$interval, 
    xlab = "Interval", ylab="Average Steps", col= rainbow(20))   # plots the average steps taken in each interval across all the days 
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
    
===============================================================================================================    

This code chunk shows the max value of steps per interval and the interval it is associated with


```r
avg_steps_per_interval_wo_NA <- arrange(avg_steps_per_interval_wo_NA, desc(avg_steps_per_interval))    
head(select(avg_steps_per_interval_wo_NA, interval, avg_steps_per_interval),1)
```

```
## Source: local data frame [1 x 2]
## 
##   interval avg_steps_per_interval
##      (int)                  (dbl)
## 1      835               206.1698
```

===============================================================================================================  

Question 3. Imputing missing values

   1. Calculate and report the total number of missing values in the dataset (i.e. the total number of         rows with NAs)
   2. Devise a strategy for filling in all of the missing values in the dataset. 
   3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
   4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and       median total number of steps taken per day. 
   5. Do these values differ from the estimates from the first part of the assignment? 
   6. What is the impact of imputing missing data on the estimates of the total daily number of steps?
   
=================================================================================================================


```r
numberofNAs = sum(is.na(activitydata$steps))
print (cat("Number of NA's in original data set: ", numberofNAs))
```

```
## Number of NA's in original data set:  2304NULL
```

```r
print ("Now to fill the NAs in with the average steps for the interval") 
```

```
## [1] "Now to fill the NAs in with the average steps for the interval"
```

```r
avg_steps_per_interval_wo_NA <- arrange(avg_steps_per_interval_wo_NA, interval)  #rearrange average steps per day into ascending order
activitydatanareplaced <- activitydata    # make new data set to fill in NAs
rowsactivitydata = nrow(activitydata) 
 
for (i in 1:rowsactivitydata) {       #replaces NA values in steps with average steps for the interval  
    if (is.na(activitydatanareplaced$steps[i])) {
        activitydatanareplaced$steps[i] <-  avg_steps_per_interval_wo_NA$avg_steps_per_interval[match(activitydatanareplaced$interval[i],avg_steps_per_interval_wo_NA$interval)]
    }     
}

totalstepsperday_w_NA  <- activitydatanareplaced %>%
    group_by(date) %>% 
    summarize(stepsperday = sum(steps, na.rm = TRUE)) #calculate steps per day     
 
g2<-ggplot(totalstepsperday_w_NA, aes(stepsperday)) + geom_histogram(binwidth=500, fill=NA, color="blue") + 
        theme_bw() + ylab("Number of Days") + xlab("Steps per Day") 
grid.arrange(g2, nrow=1) 
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
meantotalstepsperday_w_NA <- mean(totalstepsperday_w_NA$stepsperday)
summary(totalstepsperday_w_NA$stepsperday)   # this produces a summary of statistics related to steps per day with NAs replaced including the mean     
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

```r
summary(totalstepsperday_wo_NA$stepsperday)   # this produces a summary of statistics related to steps per day without NAs replaced including the mean        
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

=================================================================================================================
Q3 Report:
The mean an median of the data set where NA steps are replaced is higher than the mean of the data set where the NAs are not replaced.
In addition, the median and mean in the data set with NA steps replaced are the same where this is not the case with the data set excluding NAs
The third quartile and max values of steps per day are the same in each data set     

=============================================================================================================== 


Question 4. Are there differences in activity patterns between weekdays and weekends?

   1.	Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating         whether a given date is a weekday or weekend day.
   2.	Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)       and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) 

======================================================================================================================

Code chunk to plot comparison of steps per interval weekdays vs. weekends


```r
activitydatanareplaced <- mutate(activitydatanareplaced, dayofweek = wday(activitydatanareplaced$date))
activitydatanareplaced <- mutate(activitydatanareplaced, dayofweekII = ifelse(dayofweek == 1 | dayofweek == 2 | dayofweek == 3 |dayofweek == 4 | dayofweek == 5, "weekday",
                            ifelse(dayofweek == 6 | dayofweek == 7, "weekend",NA)))

avg_steps_per_interval_w_NA  <- activitydatanareplaced %>%
    group_by(interval,dayofweekII) %>% 
    summarize(avg_steps_per_interval = mean(steps, na.rm = TRUE)) #create average steps per 5 minute interval 
 
g3<-ggplot(avg_steps_per_interval_w_NA, aes(x = interval, y = avg_steps_per_interval, group = dayofweekII)) +
    geom_line() +
    geom_line(mapping = aes(y = avg_steps_per_interval), lty = "solid", color="blue") +
    xlab("Interval") +
    ylab("Average Steps") +
    ggtitle("Steps - Weekdays vs Weekends") +     
    facet_wrap( ~ dayofweekII)
grid.arrange(g3, nrow=1)                             
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

======================================================================================================================

Code chunk to caclulate and print average steps per day - weekend vs.weekday


```r
tot_steps_day <- activitydatanareplaced %>%
    group_by(date) %>% 
    summarize(tot_steps_day = sum(steps, na.rm = TRUE)) #calculate average steps per day weekend/weekday
    
tot_steps_day <- mutate(tot_steps_day, dayofweek = wday(tot_steps_day$date))
tot_steps_day <- mutate(tot_steps_day, dayofweekII = ifelse(dayofweek == 1 | dayofweek == 2 | dayofweek == 3 |dayofweek == 4 | dayofweek == 5, "weekday",
                            ifelse(dayofweek == 6 | dayofweek == 7, "weekend",NA)))

wewd_avg_steps_per_day  <- tot_steps_day %>%
    group_by(dayofweekII) %>% 
    summarize(avg_steps_day = mean(tot_steps_day, na.rm = TRUE)) #create average steps per weekday or weekend day    

print (cat("The average steps taken on the weekend:", wewd_avg_steps_per_day$avg_steps_day[2], " is higher than on weekdays:", wewd_avg_steps_per_day$avg_steps_day[1]))    
```

```
## The average steps taken on the weekend: 12150.86  is higher than on weekdays: 10231.2NULL
```

=======================================================================================================================   
