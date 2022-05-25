#'*AB-207 Project:* 
#'*Behavioral Ecology of the Svalbard Reindeer* 
#'*Spring 2022, at the University Centre in Svalbard*

library(tidyverse)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(irr)

dat <- read.csv("Data/Scan_sampling_data_2022.csv")
weatherdat <- read.csv("Data/Weather_data_per_hour_2022.csv")


####'*Date-time formatting*
#Making time/date column
dat$date_time = ymd_hm(paste(dat$Date, dat$Time))
#dat$numericDatetime = as.numeric(as.POSIXct(dat$data$Time),

#Getting the weather data to have the same date/time format
names(weatherdat) [3] <- "date_time"

weatherdat$date_time <- as.POSIXct(weatherdat$date_time, "%d.%m.%Y %H:%M", tz = "UTC")


####'*Filtering and subsetting data*
#Filtering data by sex
dat %>%
  filter(Sex == 1) -> maledf
dat %>%
  filter(Sex == 0) -> femaledf
dat %>%
  filter(Sex == 99) -> otherdf

#Group by hour
hourlyDat <- dat %>%
  mutate(Time = date_time) %>%
  mutate(date_time = ceiling_date(date_time, unit = "hour")) %>%
  group_by(date_time, HerdID) %>%
  summarise(`sum(Foraging[Foraging > 0])` = sum(Foraging[Foraging > 0]),
            `sum(Resting)` = sum(Resting),
            `sum(Ruminating)` = sum(Ruminating),
            `sum(Walking)` = sum(Walking),
            `sum(Displaying)` = sum(Displaying),
            `sum(Other)` = sum(Other)) %>%
  ungroup()

groupedDat <- dat %>%
  mutate(Time = date_time) %>%
  mutate(date_time = date_time) %>%
  group_by(date_time, HerdID) %>%
  summarise(`sum(Foraging[Foraging > 0])` = sum(Foraging[Foraging > 0]),
            `sum(Resting)` = sum(Resting),
            `sum(Ruminating)` = sum(Ruminating),
            `sum(Walking)` = sum(Walking),
            `sum(Displaying)` = sum(Displaying),
            `sum(Other)` = sum(Other)) %>%
  ungroup()

hourlyfemaledf <- femaledf %>%
  mutate(Time = date_time) %>%
  mutate(date_time = ceiling_date(date_time, unit = "hour")) %>%
  group_by(date_time, HerdID) %>%
  summarise(`sum(Foraging[Foraging > 0])` = sum(Foraging[Foraging > 0]),
            `sum(Resting)` = sum(Resting),
            `sum(Ruminating)` = sum(Ruminating),
            `sum(Walking)` = sum(Walking),
            `sum(Displaying)` = sum(Displaying),
            `sum(Other)` = sum(Other)) %>%
  ungroup()

#Changing column names
names(weatherdat) [4] <- "SnowDepth"
names(weatherdat) [5] <- "Precipitation"
names(weatherdat) [6] <- "AirTemp"
names(weatherdat) [7] <- "MeanWindSpeed"

names(hourlyDat) <- c("date_time", "HerdID", "TotalForaging", "TotalResting", 
                           "TotalRuminating", "TotalWalking", "TotalDisplaying",
                           "TotalOther")

names(hourlyfemaledf) <- c("date_time", "HerdID", "TotalForaging", "TotalResting", 
                           "TotalRuminating", "TotalWalking", "TotalDisplaying",
                           "TotalOther")

#colnames(weatherdat) <- c("date_time", "Location", "Station", "SnowDepth", 
                          #"Precipitation", "AirTemperature", "MeanWindSpeed")

#Arrange by time
arrmaledf <- maledf %>%
  mutate(date_time,
         hour = hour(date_time),
         min = minute(date_time),
         sec = second(date_time)) %>%
  arrange(hour, min, sec)
  #select(date_time)

#Filtering weather data to only include relvant date/times

unique(hourlyfemaledf$date_time)

weatherSpecificF <- subset(weatherdat, subset = date_time %in% c(unique(hourlyfemaledf$date_time)))

weatherSample <- merge(weatherFSpecific, hourlyfemaledf, by="date_time")

weatherSpecific <- subset(weatherdat, subset = date_time %in% c(unique(hourlyDat$date_time)))

complete_hourly_dat <- merge(weatherSpecific, hourlyDat, by="date_time")

#Adding a column for total amount of individuals per hour
complete_hourly_dat$TotalIndividuals = rowSums(complete_hourly_dat
                                            [,c("TotalForaging", "TotalResting", 
                                                "TotalRuminating","TotalWalking", 
                                                "TotalDisplaying", "TotalOther")])



# Plotting weather data ---------------------------------------------------
ggplot(weatherdat, mapping = aes(x=date_time, y=AirTemp)) +
  geom_line() +
  labs (y= "Air temperature") + 
  labs (x= "Time") 
  



# Plotting activity data --------------------------------------------------
####'*Plotting data*
ggplot(hourlyfemaledf, mapping = aes(x=date_time, y=TotalForaging)) +
geom_point(aes(group = factor(HerdID), color = factor(HerdID))) +
  

ggplot(complete_hourly_dat, mapping = aes(x=date_time, y=TotalResting)) +
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))

ggplot(complete_hourly_dat, mapping = aes(x=date_time, y=TotalResting)) +
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))

ggplot(complete_hourly_dat, mapping = aes(x=AirTemp, y=TotalForaging)) +
  geom_point(aes(group = factor(HerdID), color = factor(HerdID)))

ggplot(complete_hourly_dat, mapping = aes(x=date_time, y=AirTemp)) +
  geom_point()

##Percentage of time spent each activity total----
total_complete_hourly_dat <- complete_hourly_dat %>%
summarise(`sum(TotalForaging)` = sum(TotalForaging),
          `sum(TotalResting)` = sum(TotalResting),
          `sum(TotalRuminating)` = sum(TotalRuminating),
          `sum(TotalWalking)` = sum(TotalWalking),
          `sum(TotalDisplaying)` = sum(TotalDisplaying),
          `sum(TotalOther)` = sum(TotalOther),
          `sum(TotalIndividuals)` = sum(TotalIndividuals)) %>%
  ungroup()

ggplot(total_complete_hourly_dat, 
       aes(x=Løsning,y=, fill = Løsning, alpha = Frass))+
  geom_bar(stat = "identity", position = position_dodge())
  #scale_alpha_discrete(range=c(0.4, 1)) +
  #labs (y= "type in a y axis label here") +
  #theme(panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(),
        #panel.background = element_blank())


####'*Multiple linear regression*
# Testing if weather has an effect on total individuals resting/ruminating/foraging
cor.dat <- na.omit(complete_hourly_dat)
cor.test<-data.matrix(cor.dat[c("SnowDepth","Precipitation","AirTemp",
                                "MeanWindSpeed","TotalResting","TotalForaging","TotalRuminating")])

cor.coef <- cor(cor.test)
cor.coef

m5 <- lm(TotalForaging~Precipitation+AirTemp
         +MeanWindSpeed+TotalIndividuals, data=complete_hourly_dat)
summary(m5)

m5 <- lm(TotalForaging~Precipitation+AirTemp
                    +MeanWindSpeed, data=complete_hourly_dat, subset = date_time>"2022-04-15 00:00:00")
plot(m5)
confint(m5)

# Testing if weather impacts how long they rest/ruminate/forage for



kappam.fleiss()