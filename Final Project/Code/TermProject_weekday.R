require(tidyr)
library(plotrix)
library(gplots)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(quantmod)
library(depmixS4)
library(dplyr)
library(zoo)

#getting Data
x <- read.table("TermProjectData.txt", header = TRUE, sep = ',', fill = FALSE, strip.white = TRUE)
x$Date <- as.POSIXlt(x$Date, format = "%d/%m/%Y")
x$Time <- as.POSIXlt(x$Time, format = "%H:%M:%S")
sprintf("Table read")
set.seed(1)

#============================Obtaining Data============================#
weekday <- x[which(weekdays(as.Date(x$Date, format = "%d/%m/%Y"))
                   %in% c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
#4pm - 11pm
weekday <- subset(weekday, weekday$Time >= as.POSIXlt("16:00:00", format = "%H:%M:%S") & weekday$Time <= as.POSIXlt("23:00:00", format = "%H:%M:%S"))

weekday <- na.omit(weekday) #omitting the NA values that mess with aggregate

days_training <- data.frame(NULL)
days_testing <- data.frame(NULL)

#getting subset for training and testing
days_training <- subset(weekday, weekday$Date >= as.POSIXlt("16/12/2006", format = "%d/%m/%Y") & weekday$Date <= as.POSIXlt("31/12/2008", format = "%d/%m/%Y"))
days_testing <- subset(weekday, weekday$Date >= as.POSIXlt("1/1/2009", format = "%d/%m/%Y") & weekday$Date <= as.POSIXlt("1/12/2009", format = "%d/%m/%Y"))

#============================HMM Train============================#
ntime_count = count(days_training, "Date")

#16 states - this is the best model in terms of fit -> where BIC/logLik intersect
model_16 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1, Sub_metering_3 ~ 1), data = days_training, nstates = 16, family = list(gaussian(), gaussian(), poisson()), ntimes = ntime_count$freq)
fitModel_16 <- fit(model_16)
summary(fitModel_16)
print(fitModel_16)

#============================LogLik of Test============================#
ntime_count_test = count(days_testing, "Date")
#16 states
test_model_16 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1, Sub_metering_3 ~ 1), data = days_testing, nstates = 16, family = list(gaussian(), gaussian(), poisson()), ntimes = ntime_count$freq)
#usage: setpars(testModel, getpars(fit_trainModel))
testMod <- setpars(test_model_16, getpars(fitModel_16))

fbTest <- forwardbackward(testMod)
train_logLik <- logLik(fitModel_16)
test_logLik <- logLik(testMod)
#======================================================================#




#============================ANOMALY DATA============================#
#====Dataset 1====#
anomalyData1 <- read.table("Data1(WithAnomalies).txt", header = TRUE, sep = ',', fill = FALSE, strip.white = TRUE)
anomalyData1$Date <- as.POSIXlt(anomalyData1$Date, format = "%d/%m/%Y")
anomalyData1$Time <- as.POSIXlt(anomalyData1$Time, format = "%H:%M:%S")
sprintf("finished reading Data1(WithAnomalies).txt")

#getting the weekdays in the anomaly data, 4-11pm
anomalyData1 <- anomalyData1[which(weekdays(as.Date(anomalyData1$Date, format = "%d/%m/%Y")) %in% c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
anomalyData1 <- subset(anomalyData1, anomalyData1$Time >= as.POSIXlt("16:00:00", format = "%H:%M:%S") & anomalyData1$Time <= as.POSIXlt("23:00:00", format = "%H:%M:%S"))
anomalyData1 <- na.omit(anomalyData1)

ntime_count = count(anomalyData1, "Date")
model_anom1 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1, Sub_metering_3 ~ 1), data = anomalyData1, nstates = 16, family = list(gaussian(), gaussian(), poisson()), ntimes = ntime_count$freq)
newAnom1 <- setpars(model_anom1, getpars(fitModel_16))

fbAnom1 <- forwardbackward(newAnom1)
anom1LogLik <-logLik(newAnom1)


#====Dataset 2====#
anomalyData2 <- read.table("Data2(WithAnomalies).txt", header = TRUE, sep = ',', fill = FALSE, strip.white = TRUE)
anomalyData2$Date <- as.POSIXlt(anomalyData2$Date, format = "%d/%m/%Y")
anomalyData2$Time <- as.POSIXlt(anomalyData2$Time, format = "%H:%M:%S")
sprintf("finished reading Data2(WithAnomalies).txt")

#getting the weekdays in the anomaly data, 4-11pm
anomalyData2 <- anomalyData2[which(weekdays(as.Date(anomalyData2$Date, format = "%d/%m/%Y")) %in% c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
anomalyData2 <- subset(anomalyData2, anomalyData2$Time >= as.POSIXlt("16:00:00", format = "%H:%M:%S") & anomalyData2$Time <= as.POSIXlt("23:00:00", format = "%H:%M:%S"))
anomalyData2 <- na.omit(anomalyData2)

ntime_count = count(anomalyData2, "Date")
model_anom2 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1, Sub_metering_3 ~ 1), data = anomalyData2, nstates = 16, family = list(gaussian(), gaussian(), poisson()), ntimes = ntime_count$freq)
newAnom2 <- setpars(model_anom2, getpars(fitModel_16))

fbAnom2 <- forwardbackward(newAnom2)
anom2LogLik <- logLik(newAnom2)


#====Dataset 3====#
anomalyData3 <- read.table("Data3(WithAnomalies).txt", header = TRUE, sep = ',', fill = FALSE, strip.white = TRUE)
anomalyData3$Date <- as.POSIXlt(anomalyData3$Date, format = "%d/%m/%Y")
anomalyData3$Time <- as.POSIXlt(anomalyData3$Time, format = "%H:%M:%S")
sprintf("finished reading Data3(WithAnomalies).txt")

#getting the weekdays in the anomaly data, 4-11pm
anomalyData3 <- anomalyData3[which(weekdays(as.Date(anomalyData3$Date, format = "%d/%m/%Y")) %in% c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
anomalyData3 <- subset(anomalyData3, anomalyData3$Time >= as.POSIXlt("16:00:00", format = "%H:%M:%S") & anomalyData3$Time <= as.POSIXlt("23:00:00", format = "%H:%M:%S"))
anomalyData3 <- na.omit(anomalyData3)

ntime_count = count(anomalyData3, "Date")
model_anom3 <- depmix(list(Global_active_power ~ 1, Global_intensity ~ 1, Sub_metering_3 ~ 1), data = anomalyData3, nstates = 16, family = list(gaussian(), gaussian(), poisson()), ntimes = ntime_count$freq)
newAnom3 <- setpars(model_anom3, getpars(fitModel_16))

fbAnom3 <- forwardbackward(newAnom3)
anom3LogLik <- logLik(newAnom3)


#============================COMPARING LOGLIK============================#
sprintf("LOG-LIKELIHOODS")
sprintf("Train: %f", train_logLik)
sprintf("Test: %f", test_logLik)
sprintf("Anomaly1: %f", anom1LogLik)
sprintf("Anomaly2: %f", anom2LogLik)
sprintf("Anomaly3: %f", anom3LogLik)

sprintf("forwardbackward() LOG-LIKELIHOODS")
sprintf("Test: %f", as.numeric(fbTest[6]))
sprintf("Anomaly1: %f", as.numeric(fbAnom1[6]))
sprintf("Anomaly2: %f", as.numeric(fbAnom2[6]))
sprintf("Anomaly3: %f", as.numeric(fbAnom3[6]))

sprintf("NORMALIZED LOG-LIKELIHOODS")
sprintf("Train: %f", train_logLik / nrow(days_training))
sprintf("Test: %f", test_logLik / nrow(days_testing))
sprintf("Anomaly1: %f", anom1LogLik / nrow(anomalyData1))
sprintf("Anomaly2: %f", anom2LogLik / nrow(anomalyData2))
sprintf("Anomaly3: %f", anom3LogLik / nrow(anomalyData3))



#===========================MA ANOMALY DETECTION============================#

#=================SETTING UP XTS for Global Active Power=================#
trainXTS <- xts(days_training[,3], order.by = as.Date(days_training[,1]))
anom1XTS <- xts(anomalyData1[,3], order.by = as.Date(anomalyData1[,1]))
anom2XTS <- xts(anomalyData2[,3], order.by = as.Date(anomalyData2[,1]))
anom3XTS <- xts(anomalyData3[,3], order.by = as.Date(anomalyData3[,1]))


#===MOvING AVERAGE TRAIN===
library(pracma)
max = max(trainXTS)
Threshold1 = 0.25 * max
Threshold2 = 0.5 * max
Threshold3= 0.75 * max
anomaliesGAA <- matrix( nrow = 3, ncol = 4)
colnames(anomaliesGAA) = c("Train", "Anomaly", "Anomaly2", "Anomaly3")
rownames(anomaliesGAA) = c("Threshold1","Threshold2","Threshold3")

train = movavg(trainXTS, 10, "e")
difference = abs(trainXTS - train)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesGAA[1,1] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesGAA[2,1] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesGAA[3,1] = tabulate(difference3)

#===MOvING AVERAGE ANOMALY1===#
anomaly1 = movavg(anom1XTS, 10, "e")
difference = abs(anom1XTS - anomaly1)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesGAA[1,2] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesGAA[2,2] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesGAA[3,2] = tabulate(difference3)

# #===MOvING AVERAGE ANOMALY2===#
anomaly2 = movavg(anom2XTS, 10, "e")
difference = abs(anom2XTS - anomaly2)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesGAA[1,3] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesGAA[2,3] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesGAA[3,3] = tabulate(difference3)

# #===MOvING AVERAGE ANOMALY3===#
anomaly3 = movavg(anom3XTS, 10, "e")
difference = abs(anom3XTS - anomaly1)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesGAA[1,4] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesGAA[2,4] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesGAA[3,4] = tabulate(difference3)

#===Plotting Anomalies===#
dat = days_training[,3]
anom1 = anomalyData1[,3]
anom2 = anomalyData2[,3]
anom3 = anomalyData3[,3]
mindat = mean(dat, na.rm = T) - 4*sd(dat, na.rm = T)
maxdat = mean(dat, na.rm = T) + 4*sd(dat, na.rm = T)
minanom1 = mean(anom1, na.rm = T) - 4*sd(anom1, na.rm = T)
maxanom1 = mean(anom1, na.rm = T) + 4*sd(anom1, na.rm = T)
minanom2 = mean(anom2, na.rm = T) - 4*sd(anom2, na.rm = T)
maxanom2 = mean(anom2, na.rm = T) + 4*sd(anom2, na.rm = T)
minanom3 = mean(anom3, na.rm = T) - 4*sd(anom3, na.rm = T)
maxanom3 = mean(anom3, na.rm = T) + 4*sd(anom3, na.rm = T)

# #===Mean-SD Train===#
position = data.frame(id=seq(1, length(dat)), value=dat)
anomalyH = position[position$value > maxdat, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < mindat, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]


plot(as.ts(dat), ylim =c(-5,10))
real = data.frame(id=seq(1, length(dat)), value=dat)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxdat, col= "blue", lwd=2)
abline(h=mindat, col="blue", lwd=2)

# #===Mean-SD Anom1===#
position = data.frame(id=seq(1, length(anom1)), value=anom1)
anomalyH = position[position$value > maxanom1, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom1, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom1), ylim =c(-5,10))
real = data.frame(id=seq(1, length(anom1)), value=anom1)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom1, col="yellow", lwd=2)
abline(h=minanom1, col="yellow", lwd=2)

# #===Mean-SD Anom2===#
position = data.frame(id=seq(1, length(anom2)), value=anom2)
anomalyH = position[position$value > maxanom2, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom2, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom2), ylim =c(-10,25))
real = data.frame(id=seq(1, length(anom2)), value=anom2)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom2, col= "green", lwd=2)
abline(h=minanom2, col="green", lwd=2)

# #===Mean-SD Anom3===#
position = data.frame(id=seq(1, length(anom3)), value=anom3)
anomalyH = position[position$value > maxanom3, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom3, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom3), ylim =c(-3,13))
real = data.frame(id=seq(1, length(anom3)), value=anom3)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom3, col= "cyan", lwd=2)
abline(h=minanom3, col="cyan", lwd=2)


#====================SETTING UP XTS for Global Intensity-================#
trainXTS <- xts(days_training[,6], order.by = as.Date(days_training[,1]))
anom1XTS <- xts(anomalyData1[,6], order.by = as.Date(anomalyData1[,1]))
anom2XTS <- xts(anomalyData2[,6], order.by = as.Date(anomalyData2[,1]))
anom3XTS <- xts(anomalyData3[,6], order.by = as.Date(anomalyData3[,1]))


#===MOvING AVERAGE TRAIN===
library(pracma)
max = max(trainXTS)
Threshold1 = 0.25 * max
Threshold2 = 0.5 * max
Threshold3= 0.75 * max
anomaliesGI <- matrix( nrow = 3, ncol = 4)
colnames(anomaliesGI) = c("Train", "Anomaly", "Anomaly2", "Anomaly3")
rownames(anomaliesGI) = c("Threshold1","Threshold2","Threshold3")

train = movavg(trainXTS, 10, "e")
difference = abs(trainXTS - train)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesGI[1,1] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesGI[2,1] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesGI[3,1] = tabulate(difference3)

#===MOvING AVERAGE ANOMALY1===#
anomaly1 = movavg(anom1XTS, 10, "e")
difference = abs(anom1XTS - anomaly1)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesGI[1,2] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesGI[2,2] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesGI[3,2] = tabulate(difference3)

# #===MOvING AVERAGE ANOMALY2===#
anomaly2 = movavg(anom2XTS, 10, "e")
difference = abs(anom2XTS - anomaly2)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesGI[1,3] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesGI[2,3] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesGI[3,3] = tabulate(difference3)

# #===MOvING AVERAGE ANOMALY3===#
anomaly3 = movavg(anom3XTS, 10, "e")
difference = abs(anom3XTS - anomaly1)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesGI[1,4] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesGI[2,4] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesGI[3,4] = tabulate(difference3)

#===Plotting Anomalies===#
dat = days_training[,6]
anom1 = anomalyData1[,6]
anom2 = anomalyData2[,6]
anom3 = anomalyData3[,6]
mindat = mean(dat, na.rm = T) - 4*sd(dat, na.rm = T)
maxdat = mean(dat, na.rm = T) + 4*sd(dat, na.rm = T)
minanom1 = mean(anom1, na.rm = T) - 4*sd(anom1, na.rm = T)
maxanom1 = mean(anom1, na.rm = T) + 4*sd(anom1, na.rm = T)
minanom2 = mean(anom2, na.rm = T) - 4*sd(anom2, na.rm = T)
maxanom2 = mean(anom2, na.rm = T) + 4*sd(anom2, na.rm = T)
minanom3 = mean(anom3, na.rm = T) - 4*sd(anom3, na.rm = T)
maxanom3 = mean(anom3, na.rm = T) + 4*sd(anom3, na.rm = T)

# #===Mean-SD Train===#
position = data.frame(id=seq(1, length(dat)), value=dat)
anomalyH = position[position$value > maxdat, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < mindat, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]


plot(as.ts(dat), ylim =c(0,45))
real = data.frame(id=seq(1, length(dat)), value=dat)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxdat, col= "blue", lwd=2)
abline(h=mindat, col="blue", lwd=2)


# #===Mean-SD Anom1===#
position = data.frame(id=seq(1, length(anom1)), value=anom1)
anomalyH = position[position$value > maxanom1, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom1, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom1), ylim =c(0,40))
real = data.frame(id=seq(1, length(anom1)), value=anom1)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom1, col="yellow", lwd=2)
abline(h=minanom1, col="yellow", lwd=2)

# #===Mean-SD Anom2===#
position = data.frame(id=seq(1, length(anom2)), value=anom2)
anomalyH = position[position$value > maxanom2, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom2, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom2), ylim =c(0,100))
real = data.frame(id=seq(1, length(anom2)), value=anom2)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom2, col= "green", lwd=2)
abline(h=minanom2, col="green", lwd=2)

# #===Mean-SD Anom3===#
position = data.frame(id=seq(1, length(anom3)), value=anom3)
anomalyH = position[position$value > maxanom3, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom3, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom3), ylim =c(0,35))
real = data.frame(id=seq(1, length(anom3)), value=anom3)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom3, col= "cyan", lwd=2)
abline(h=minanom3, col="cyan", lwd=2)

#==================SETTING UP XTS for Submetering3===================#
trainXTS <- xts(days_training[,9], order.by = as.Date(days_training[,1]))
anom1XTS <- xts(anomalyData1[,9], order.by = as.Date(anomalyData1[,1]))
anom2XTS <- xts(anomalyData2[,9], order.by = as.Date(anomalyData2[,1]))
anom3XTS <- xts(anomalyData3[,9], order.by = as.Date(anomalyData3[,1]))


#===MOvING AVERAGE TRAIN===
library(pracma)
max = max(trainXTS)
Threshold1 = 0.25 * max
Threshold2 = 0.5 * max
Threshold3= 0.75 * max
anomaliesS3 <- matrix( nrow = 3, ncol = 4)
colnames(anomaliesS3) = c("Train", "Anomaly", "Anomaly2", "Anomaly3")
rownames(anomaliesS3) = c("Threshold1","Threshold2","Threshold3")

train = movavg(trainXTS, 10, "e")
difference = abs(trainXTS - train)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesS3[1,1] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesS3[2,1] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesS3[3,1] = tabulate(difference3)

#===MOvING AVERAGE ANOMALY1===#
anomaly1 = movavg(anom1XTS, 10, "e")
difference = abs(anom1XTS - anomaly1)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesS3[1,2] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesS3[2,2] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesS3[3,2] = tabulate(difference3)

# #===MOvING AVERAGE ANOMALY2===#
anomaly2 = movavg(anom2XTS, 10, "e")
difference = abs(anom2XTS - anomaly2)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesS3[1,3] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesS3[2,3] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesS3[3,3] = tabulate(difference3)

# #===MOvING AVERAGE ANOMALY3===#
anomaly3 = movavg(anom3XTS, 10, "e")
difference = abs(anom3XTS - anomaly1)
difference1 <-ifelse((difference > Threshold1), 1, 0)
anomaliesS3[1,4] = tabulate(difference1)
difference2 <-ifelse((difference > Threshold2), 1, 0)
anomaliesS3[2,4] = tabulate(difference2)
difference3 <-ifelse((difference > Threshold3), 1, 0)
anomaliesS3[3,4] = tabulate(difference3)

#===Plotting Anomalies===#
dat = days_training[,9]
anom1 = anomalyData1[,9]
anom2 = anomalyData2[,9]
anom3 = anomalyData3[,9]
mindat = mean(dat, na.rm = T) - 2.5*sd(dat, na.rm = T)
maxdat = mean(dat, na.rm = T) + 2.5*sd(dat, na.rm = T)
minanom1 = mean(anom1, na.rm = T) - 2.5*sd(anom1, na.rm = T)
maxanom1 = mean(anom1, na.rm = T) + 2.5*sd(anom1, na.rm = T)
minanom2 = mean(anom2, na.rm = T) - 2.5*sd(anom2, na.rm = T)
maxanom2 = mean(anom2, na.rm = T) + 2.5*sd(anom2, na.rm = T)
minanom3 = mean(anom3, na.rm = T) - 2.5*sd(anom3, na.rm = T)
maxanom3 = mean(anom3, na.rm = T) + 2.5*sd(anom3, na.rm = T)

# #===Mean-SD Train===#
position = data.frame(id=seq(1, length(dat)), value=dat)
anomalyH = position[position$value > maxdat, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < mindat, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]


plot(as.ts(dat), ylim =c(0,45))
real = data.frame(id=seq(1, length(dat)), value=dat)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxdat, col= "blue", lwd=2)
abline(h=mindat, col="blue", lwd=2)

# #===Mean-SD Anom1===#
position = data.frame(id=seq(1, length(anom1)), value=anom1)
anomalyH = position[position$value > maxanom1, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom1, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom1), ylim =c(0,40))
real = data.frame(id=seq(1, length(anom1)), value=anom1)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom1, col="yellow", lwd=2)
abline(h=minanom1, col="yellow", lwd=2)


# #===Mean-SD Anom2===#
position = data.frame(id=seq(1, length(anom2)), value=anom2)
anomalyH = position[position$value > maxanom2, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom2, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom2), ylim =c(0,100))
real = data.frame(id=seq(1, length(anom2)), value=anom2)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom2, col= "green", lwd=2)
abline(h=minanom2, col="green", lwd=2)

# #===Mean-SD Anom3===#
position = data.frame(id=seq(1, length(anom3)), value=anom3)
anomalyH = position[position$value > maxanom3, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < minanom3, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id),
                     value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]

plot(as.ts(anom3), ylim =c(0,35))
real = data.frame(id=seq(1, length(anom3)), value=anom3)
realAnomaly = real[anomaly$id, ]
points(x = realAnomaly$id, y =realAnomaly$value, col="#e15f3f")
abline(h=maxanom3, col= "cyan", lwd=2)
abline(h=minanom3, col="cyan", lwd=2)