# AB 207 2022
library(tidyverse)
library(lubridate)
library(irr)
library(gam)
library(matrixStats)

# Behavior data
data <- read.csv("C:/Users/lyond/Documents/UNIS22/AB207/Scan_sampling_data_2022.csv", na.strings = "NA")
attach(data)

#### FIX DATE and TIME
# Sys.setenv(TZ = "Arctic/Longyearbyen")
# now()

# make datetime column
data$date_time <- ymd_hm(paste(data$Date, data$Time))
# data$numericDateTime <- as.numeric(as.POSIXct(data$date_time))
# plot(date_time, HerdSize)
# plot(date_time, Foraging)

# Weather data
weatherDat <- read.csv("C:/Users/lyond/Documents/UNIS22/AB207/Weather_data_per_hour_2022.csv", na.strings = "NA")

# fix datetime
weatherDat$date_time <- as.POSIXct(weatherDat$Time.norwegian.mean.time., "%d.%m.%Y %H:%M", tz = "UTC")

# Make a data framefor each sex
data %>%
  filter(Sex == 1) -> maledf

data %>%
  filter(Sex == 0) -> femaledf

data %>%
  filter(Sex == 99) -> sexUDdf

# plot stuff for females
plot(femaledf$date_time, femaledf$HerdSize)
plot(femaledf$date_time, femaledf$Resting)

# females, March 31
femaleMarch31 <- subset(femaledf, date_time >= as.POSIXct("2022-03-31 00:00") &
                         date_time <= as.POSIXct("2022-03-31 23:59"))
plot(femaleMarch31$date_time, femaleMarch31$Resting)

ggplot(femaleMarch31, aes(x = date_time, y = Resting))+
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))


# try to make totals for females over 1 hour periods, need to figure out foraging
fbyhour <- femaledf %>%
  mutate(Time = date_time) %>%
  mutate(date_time = ceiling_date(date_time, unit = "hour")) %>%
  group_by(date_time, HerdID) %>%
  summarise(`HerdID`  = HerdID,
            `HerdSize` = HerdSize,
            `sum(Foraging[Foraging > 0])` = sum(Foraging[Foraging > 0]),
            `sum(Ruminating)` = sum(Ruminating),
            `sum(Resting)` = sum(Resting), 
            `sum(Walking)` = sum(Walking),
            `sum(Displaying)` = sum(Displaying),
            `sum(Other)` = sum(Other)) %>%
  ungroup()
fbyhour

# names(fbyhour)[3] <- "totalResting"
# names(fbyhour)[4] <- "totalRuminating"
names(fbyhour) <- c("date_time", "HerdID", "HerdSize", "totalForaging", 
                    "totalRuminating", "totalResting", "totalWalking", 
                    "totalDisplaying", "totalOther")

# try to plot
ggplot(fbyhour, aes(x = date_time, y = totalRuminating))+
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))

ggplot(fbyhour, aes(x = date_time, y = totalRuminating)) + geom_point()

ggplot(fbyhour, aes(x = date_time)) + 
  geom_point(aes(y = totalRuminating)) +
  geom_point(aes(y = totalResting), color = 'red') +
  geom_point(aes(y = totalForaging), color = 'blue')

# combine these 3 into one figure? or use Menno's bar plot format
# we could subset into single sampling periodsandplot for each
ggplot(fbyhour, aes(x = date_time)) + 
  geom_point(aes(y = totalRuminating))

ggplot(fbyhour, aes(x = date_time)) +
  geom_point(aes(y = totalResting), color = 'red') 

ggplot(fbyhour, aes(x = date_time)) +
  geom_point(aes(y = totalForaging), color = 'blue')


# fbyhour$avgRuminating <- mean()

###'*BOXPLOT*
boxplot(totalRuminating ~ date_time, data = fbyhour)

ggplot(fbyhour, aes(x = date_time, y = totalRuminating, group = date_time))+
  geom_boxplot() #FIXME

# get times in order and get rid of days and times with no data
# Female
arrfemaledf <- femaledf %>%
  mutate(date_time,
         hour = hour(date_time),
         min = minute(date_time),
         sec = second(date_time)) %>%
  arrange(hour, min, sec) 

ggplot(arrfemaledf, aes(x = date_time, y = Resting))+
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))
  

# Male
arrmaledf <- maledf %>%
  mutate(date_time,
         hour = hour(date_time),
         min = minute(date_time),
         sec = second(date_time)) %>%
  arrange(hour, min, sec) 

ggplot(arrmaledf, aes(x = Time, y = Resting))+
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))

# Unknown
arrUDdf <- sexUDdf %>%
  mutate(date_time,
         hour = hour(date_time),
         min = minute(date_time),
         sec = second(date_time)) %>%
  arrange(hour, min, sec) 

ggplot(arrUDdf, aes(x = Time, y = Resting))+
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))


### Play with subsetting weather to the date times in fbyhour
unique(fbyhour$date_time)

####'*I think this is where I added Linnea's stuff*

# weatherFSpecific <- subset(weatherDat, subset = date_time %in% c(unique(fbyhour$date_time))) #FIXME
#Group by hour
hourlyData <- data %>%
  mutate(Time = date_time) %>%
  mutate(date_time = ceiling_date(date_time, unit = "hour")) %>%
  group_by(date_time, HerdID) %>%
  summarise(`length(Sex[Sex == 0])` = length(Sex[Sex == 0]),
            `length(Sex[Sex == 1]))` = length(Sex[Sex == 1]),
            `length(Sex[Sex == 99])` = length(Sex[Sex == 99]),
            `length(Foraging[Foraging > 0])` = length(Foraging[Foraging > 0]),
            `sum(Resting)` = sum(Resting),
            `sum(Ruminating)` = sum(Ruminating),
            `sum(Walking)` = sum(Walking),
            `sum(Displaying)` = sum(Displaying),
            `sum(Other)` = sum(Other)) %>%
  ungroup()


#Changing column names
names(weatherDat) [4] <- "SnowDepth"
names(weatherDat) [5] <- "Precipitation"
names(weatherDat) [6] <- "AirTemp"
names(weatherDat) [7] <- "MeanWindSpeed"

names(hourlyData) <- c("date_time", "HerdID", "NumFemale", "NumMale", "NumCalfYrl", 
                       "TotalForaging", "TotalResting", "TotalRuminating", "TotalWalking", 
                       "TotalDisplaying","TotalOther")

#Filtering weather data to only include relvant date/times

unique(fbyhour$date_time)

weatherFSpecific <- subset(weatherDat, subset = date_time %in% c(unique(fbyhour$date_time)))

weatherSample <- merge(weatherFSpecific, fbyhour, by="date_time")

weatherSpecific <- subset(weatherDat, subset = date_time %in% c(unique(hourlyData$date_time)))

complete_hourly_dat <- merge(weatherFSpecific, hourlyData, by="date_time")

#Adding a column for total amount of individuals sampled per hour 
#FIXME
complete_hourly_dat$TotalIndividuals = rowSums(complete_hourly_dat
                                               [,c("TotalForaging", "TotalResting", 
                                                   "TotalRuminating","TotalWalking", 
                                                   "TotalDisplaying", "TotalOther")])

####'Plotting data
ggplot(fbyhour, mapping = aes(x=date_time, y=totalForaging)) +
  geom_point(aes(group = factor(HerdID), color = factor(HerdID))) 

ggplot(arrmaledf, mapping = aes(x=Time, y=Resting)) +
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))

ggplot(weatherScanTotal, mapping = aes(x=Mean.wind.speed, y=TotalResting)) +
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))

# ggplot(weatherScanTotal, mapping = aes(x=date_time, y=TotalResting + AirTemp))


####'Multiple linear regression
# Testing if weather has an effect on total individuals resting/ruminating/foraging
cor.dat <- na.omit(complete_hourly_dat)
cor.test<-data.matrix(cor.dat[c("SnowDepth","Precipitation","AirTemp",
                                "MeanWindSpeed","TotalResting","TotalForaging","TotalRuminating")])

cor.coef <- cor(cor.test)#Do you see any correlation coefficients with absolute values that exceed?
cor.coef

m5 <- lm(TotalResting~SnowDepth+Precipitation+AirTemp
         +MeanWindSpeed, data=complete_hourly_dat)
summary(m5)
plot(m5)
AIC(m5)
confint(m5)

m6 <- gam(TotalResting~s(SnowDepth)+s(AirTemp)
         +s(MeanWindSpeed), data=complete_hourly_dat)
summary(m6)
plot(m6, rugplot = T, se = T)
AIC(m6)
confint(m6)

m7 <- lm(TotalRuminating ~ Precipitation * AirTemp * MeanWindSpeed, data = complete_hourly_dat)
summary(m7)


# 26/05/2022
# Synchrony??
# defn of synchrony: using cov (coeff of variation)  
# cv = stddev() / mean(), a measure of variability
# synchrony = 1/cv = mean / stddev 
# calculate for every row
# library(matrixStats)
names(complete_hourly_dat)
complete_hourly_dat$StdDev <- rowSds(as.matrix(complete_hourly_dat[,c(10:15)]))
complete_hourly_dat$behaviorMean <-  rowMeans(complete_hourly_dat[,c(10:15)])
complete_hourly_dat$synchrony <- complete_hourly_dat$behaviorMean / complete_hourly_dat$StdDev

m8 <- gam(synchrony~s(TotalIndividuals), data=complete_hourly_dat)
summary(m8)
plot(m8, rugplot = T, se = T) #use gam, after a certain density, the density dep effects are not sig
AIC(m8)
confint(m8)

m9 <- gam(synchrony~s(AirTemp), data = complete_hourly_dat)
summary(m9)
plot(m9, rugplot = T, se = T)
AIC(m9)

m10 <- gam(synchrony~s(MeanWindSpeed) + s(AirTemp) + s(Precipitation), data = complete_hourly_dat)
summary(m10)
plot(m10, rugplot = T, se = T)
AIC(m10)

# make columns for percentage of females, percentage of males, etc and use as predictor variables
complete_hourly_dat$femalePercent <- complete_hourly_dat$NumFemale / complete_hourly_dat$TotalIndividuals
complete_hourly_dat$malePercent <- complete_hourly_dat$NumMale / complete_hourly_dat$TotalIndividuals
complete_hourly_dat$calfYrlPercent <- complete_hourly_dat$NumCalfYrl / complete_hourly_dat$TotalIndividuals

m11 <- gam(synchrony~s(femalePercent), data=complete_hourly_dat)
summary(m11)
plot(m11, rugplot = T, se = T) 
AIC(m11)
confint(m11)

m12 <- gam(synchrony~s(malePercent), data=complete_hourly_dat)
summary(m12)
plot(m12, rugplot = T, se = T) 
AIC(m12)
confint(m12)

m13 <- gam(synchrony~s(calfYrlPercent), data=complete_hourly_dat)
summary(m13)
plot(m13, rugplot = T, se = T) 
AIC(m13)
confint(m13) 


# TODO
# make all the linear models
# save graphs
# make table of R values for LMs and GAMs
# just write in the stats section that the GAMs were always less parsimonious than
# the lms, so we used lms from that point on


