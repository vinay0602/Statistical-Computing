#library(devtools)
#install_github("vqv/ggbiplot")
library(xts)

#getting Data
x <- read.table("TermProjectData.txt", header = TRUE, sep = ',', fill = FALSE, strip.white = TRUE)

x$Date <- as.POSIXlt(x$Date, format = "%d/%m/%Y")
x$Time <- as.POSIXlt(x$Time, format = "%H:%M:%S")

#Weekdays from 4-11pm, omitting the NA values
weekday <- x[which(weekdays(as.Date(x$Date, format = "%d/%m/%Y")) %in% c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
weekday <- subset(weekday, weekday$Time >= as.POSIXlt("16:00:00", format = "%H:%M:%S") & weekday$Time <= as.POSIXlt("23:00:00", format = "%H:%M:%S"))
weekday <- na.omit(weekday) #omitting the NA values that mess with aggregate

#making an XTS object to calculate the average values for each time series in every week
#reference: https://stackoverflow.com/questions/13915549/average-in-time-series-based-on-time-and-date-in-r
weekday.xts <- xts(x = weekday[,3:9], as.POSIXct(paste(weekday$Date, weekday$Time)))

#for endpoints the second argument is the 'period' - we can change it to be weekly, hourly, daily, etc
ep <- endpoints(weekday.xts, 'weeks')

#applying the endpoints to the XTS object that stores our weekday data, and calculate the mean of each response variable.
#should end up with 155 rows since there are 155 weeks in the dataset
mean <- period.apply(weekday.xts, ep, mean)

#plugging data in to prcomp()
pca <- prcomp(mean, scale=TRUE)
pca
summary(pca)

#ranking them by their absolute values, printing them out in order
#reference: https://www.youtube.com/watch?v=0Jp4gsfOLMs&&ab_channel=StatQuestwithJoshStarmer
loading_scores <- pca$rotation[,1] #PCA1 accounts for highest variation
response_scores <- abs(loading_scores)
ranked <- sort(response_scores, decreasing = TRUE)
top_7 <- names(ranked)
top_7
