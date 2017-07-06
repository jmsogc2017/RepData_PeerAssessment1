Untitled
================

library(ggplot2)

library(plyr)

activity &lt;- read.csv("activity.csv")

activity*d**a**y* &lt; −*w**e**e**k**d**a**y**s*(*a**s*.*D**a**t**e*(*a**c**t**i**v**i**t**y*date)) activity*D**a**t**e**T**i**m**e* &lt; −*a**s*.*P**O**S**I**X**c**t*(*a**c**t**i**v**i**t**y*date, format="%Y-%m-%d")

pulling data without NA's
-------------------------

clean &lt;- activity\[!is.na(activity$steps),\]

summarizing total steps per date
--------------------------------

sumTable &lt;- aggregate(activity*s**t**e**p**s* *a**c**t**i**v**i**t**y*date, FUN=sum ) colnames(sumTable)&lt;- c("Date", "Steps")

####### Q1.0 = What is mean total number of steps taken per day?

      # Q1.1 = Calculate the total number of steps taken per day
        totalStepsByDay<-aggregate(steps~date, activity, sum)

      # Q1.2 =  Make a histogram of the total number of steps taken each day
        hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day")

      # Q1.3 =  Calculate and report the mean and median of the total number of steps taken per day

        # Mean of Steps
          as.integer(mean(sumTable$Steps))

        # Median of Steps
          as.integer(median(sumTable$Steps))

####### Q2.0 = What is the average daily activity pattern?

library(plyr) library(ggplot2)

pulling data without nas
------------------------

clean &lt;- activity\[!is.na(activity$steps),\]

create average number of steps per interval
-------------------------------------------

intervalTable &lt;- ddply(clean, .(interval), summarize, Avg = mean(steps))

      # Q2.1 = Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) ... plot for average number of steps per interval

          p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
          p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

      # Q2.2 = Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

        ##Maximum steps by interval
          maxSteps <- max(intervalTable$Avg)
        
          # The maximum number of steps for a 5-minute interval was 206 steps.
        
        ##Which interval contains the maximum average number of steps
          intervalTable[intervalTable$Avg==maxSteps,1]
        
          # The 5-minute interval which had the maximum number of steps was the 835 interval.

####### Q3.0 = Imputing missing values

      # Q3.1 = Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
        
        ##Number of NAs in original data set
          nrow(activity[is.na(activity$steps),])

            # The total number of rows with steps = 'NA' is 2304.

      # Q3.2 = Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

            # My strategy for filling in the NA's will be to substitute the missing steps with the average 5-minute interval based on the day of the week.

              ## Create the average number of steps per weekday and interval
              avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
              
              ## Create dataset with all NA's for substitution
              nadata<- activity[is.na(activity$steps),]
              
              ## Merge NA data with average weekday interval for substitution
              newdata<-merge(nadata, avgTable, by=c("interval", "day"))

      # Q3.3 = Create a new dataset that is equal to the original dataset but with the missing data filled in.

          ## Reorder the new substituted data in the same format as the clean data set
            newdata2<- newdata[,c(6,4,1,2,5)]
            colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
            
          ##Merge the NA averages and non NA data together
            mergeData <- rbind(clean, newdata2)

      # Q3.4 = Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

        ##Create sum of steps per date to compare with Q3.1
        sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum )
        colnames(sumTable2)<- c("Date", "Steps")
        
        ## Mean of Steps with NA data taken care of
        as.integer(mean(sumTable2$Steps))
        
        ## Median of Steps with NA data taken care of
        as.integer(median(sumTable2$Steps))
        
        ## Creating the histogram of total steps per day, categorized by data set to show impact
        hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Black")
        hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Grey", add=T)
        legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("black", "grey") )
        
        # CONCLUSION ...
        # The new mean of the imputed data is 10,821 steps compared to the previous mean of 10,766 steps. The difference is 55 steps on average per day.
        # The new median of the imputed data is 11,015 steps compared to the previous median of 10,765 steps. The difference is 250 steps.
        # The overall shape of the distribution has not changed.

####### Q4.0 = Are there differences in activity patterns between weekdays and weekends?

      # Q4.1 = Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

        ## Create new category based on the days of the week
          mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

      # Q4.2 = Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
              
        library(lattice) 
        
        ## Summarize data by interval and type of day
        intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
        
        ## Plot data in a panel plot
        xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
               main="Average Steps per Interval Based on Type of Day", 
               ylab="Average Number of Steps", xlab="Interval")
        
        # CONCLUSION ...
        # Yes, there are differences in activity patterns between weekdays and weekends according to the step activity trends. 
        # The data suggests more activity is occuring on the weekends, which is most likely a result of working during the weekday in a role that requires less moving.

\`\`\`
