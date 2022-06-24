#'*AB-207 Project:* 
#'*Behavioral Ecology of the Svalbard Reindeer* 
#'*Spring 2022, at the University Centre in Svalbard*

library(tidyverse)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(irr)

df <- read.csv("Data/Scan_sampling_data_2022.csv")
dfweather <- read.csv("Data/Weather_data_per_hour_2022.csv")
activitycycle <- read.csv("Data/activity_31.csv")


# Date/time formatting ----------------------------------------------------
##Making date/time column----
df$date_time = ymd_hm(paste(df$Date, df$Time))

#Getting the weather data to have the same date/time format
names(dfweather) [3] <- "date_time"

dfweather$date_time <- as.POSIXct(dfweather$date_time, "%d.%m.%Y %H:%M", tz = "UTC")

names(dfweather) [4] <- "SnowDepth"
names(dfweather) [5] <- "Precipitation"
names(dfweather) [6] <- "AirTemp"
names(dfweather) [7] <- "MeanWindSpeed"

names(activitycycle) [1] <- "date_time"
activitycycle$date_time <- as.POSIXct(activitycycle$date_time, "%d.%m.%Y %H:%M", tz = "UTC")

# Filtering and subsetting data -------------------------------------------
##Filt. by sex----
df %>%
  filter(Sex == 1) -> dfmale
df %>%
  filter(Sex == 0) -> dffemale
df %>%
  filter(Sex == 99) -> dfother

##Create two new df's by time period----
#Late winter (March-early April)
dftp1 <- subset(df, Date < as.Date("2022-04-26"))
#Polar day (late April-May)
dftp2 <- subset(df, Date > as.Date("2022-04-01"))

##Group by hour----
#Overall data by hour
hourlydf <- df %>%
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

names(hourlydf) <- c("date_time", "HerdID", "TotalForaging", "TotalResting", 
                      "TotalRuminating", "TotalWalking", "TotalDisplaying",
                      "TotalOther")
#Time period 1 by hour
hourlytp1df <- dftp1 %>%
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

names(hourlytp1df) <- c("date_time", "HerdID", "TotalForaging", "TotalResting", 
                     "TotalRuminating", "TotalWalking", "TotalDisplaying",
                     "TotalOther")
#Time period 2 by hour
hourlytp2df <- dftp2 %>%
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

names(hourlytp2df) <- c("date_time", "HerdID", "TotalForaging", "TotalResting", 
                     "TotalRuminating", "TotalWalking", "TotalDisplaying",
                     "TotalOther")

#Female by hour
hourlyfemaledf <- dffemale %>%
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

names(hourlyfemaledf) <- c("date_time", "HerdID", "TotalForaging", "TotalResting", 
                           "TotalRuminating", "TotalWalking", "TotalDisplaying",
                           "TotalOther")

#Male by hour
hourlymaledf <- dfmale %>%
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

names(hourlymaledf) <- c("date_time", "HerdID", "TotalForaging", "TotalResting", 
                           "TotalRuminating", "TotalWalking", "TotalDisplaying",
                           "TotalOther")

#Calf/yearling by hour
hourlyotherdf <- dfother %>%
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

names(hourlyotherdf) <- c("date_time", "HerdID", "TotalForaging", "TotalResting", 
                           "TotalRuminating", "TotalWalking", "TotalDisplaying",
                           "TotalOther")


##Filt. weather data to only relevant date/times----
#Overall data
weatherSpecific <- subset(dfweather, subset = date_time %in% c(unique(hourlydf$date_time)))

hourlydf <- merge(weatherSpecific, hourlydf, by="date_time")

#Female data
weatherSpecificF <- subset(dfweather, subset = date_time %in% c(unique(hourlyfemaledf$date_time)))

hourlyfemaledf <- merge(weatherSpecificF, hourlyfemaledf, by="date_time")

#Male data
weatherSpecificM <- subset(dfweather, subset = date_time %in% c(unique(hourlymaledf$date_time)))

hourlymaledf <- merge(weatherSpecificM, hourlymaledf, by="date_time")

#Calf/yearling data
weatherSpecificO <- subset(dfweather, subset = date_time %in% c(unique(hourlyotherdf$date_time)))

hourlyotherdf <- merge(weatherSpecificO, hourlyotherdf, by="date_time")


##Total individuals per hour column----
#Overall data
hourlydf$TotalIndividuals = rowSums(hourlydf
                                            [,c("TotalForaging", "TotalResting", 
                                                "TotalRuminating","TotalWalking", 
                                                "TotalDisplaying", "TotalOther")])
#TP1 data
hourlytp1df$TotalIndividuals = rowSums(hourlytp1df
                                    [,c("TotalForaging", "TotalResting", 
                                        "TotalRuminating","TotalWalking", 
                                        "TotalDisplaying", "TotalOther")])
#TP2 data
hourlytp2df$TotalIndividuals = rowSums(hourlytp2df
                                    [,c("TotalForaging", "TotalResting", 
                                        "TotalRuminating","TotalWalking", 
                                        "TotalDisplaying", "TotalOther")])
#Female data
hourlyfemaledf$TotalIndividuals = rowSums(hourlyfemaledf
                                               [,c("TotalForaging", "TotalResting", 
                                                   "TotalRuminating","TotalWalking", 
                                                   "TotalDisplaying", "TotalOther")])
#Male data
hourlymaledf$TotalIndividuals = rowSums(hourlymaledf
                                          [,c("TotalForaging", "TotalResting", 
                                              "TotalRuminating","TotalWalking", 
                                              "TotalDisplaying", "TotalOther")])
#Calf/yearling data
hourlyotherdf$TotalIndividuals = rowSums(hourlyotherdf
                                          [,c("TotalForaging", "TotalResting", 
                                              "TotalRuminating","TotalWalking", 
                                              "TotalDisplaying", "TotalOther")])

##Active/inactive percentage----
#Overall data
hourlydf <- hourlydf %>%
  mutate(TotalActive = (TotalForaging + TotalWalking + TotalDisplaying + TotalOther),
         TotalInactive = (TotalResting + TotalRuminating))

#TP1
hourlytp1df <- hourlytp1df %>%
  mutate(TotalActive = (TotalForaging + TotalWalking + TotalDisplaying + TotalOther),
         TotalInactive = (TotalResting + TotalRuminating))
#TP2
hourlytp2df <- hourlytp2df %>%
  mutate(TotalActive = (TotalForaging + TotalWalking + TotalDisplaying + TotalOther),
         TotalInactive = (TotalResting + TotalRuminating))

#Female data
hourlyfemaledf <- hourlyfemaledf %>%
  mutate(TotalActive = (TotalForaging + TotalWalking + TotalDisplaying + TotalOther),
         TotalInactive = (TotalResting + TotalRuminating))

#Male data
hourlymaledf <- hourlymaledf %>%
  mutate(TotalActive = (TotalForaging + TotalWalking + TotalDisplaying + TotalOther),
         TotalInactive = (TotalResting + TotalRuminating))

#Calf/yearling data
hourlyotherdf <- hourlyotherdf %>%
  mutate(TotalActive = (TotalForaging + TotalWalking + TotalDisplaying + TotalOther),
         TotalInactive = (TotalResting + TotalRuminating))

##Activity percentages----
#Overall data
sumTotalIndividualsdf <- hourlydf %>%
  summarise(`sum(TotalIndividuals)` = sum(TotalIndividuals))

percenthourlydf <- hourlydf %>%
  summarise(`sum(TotalForaging)` = sum(TotalForaging)/sumTotalIndividualsdf,
            `sum(TotalResting)` = sum(TotalResting)/sumTotalIndividualsdf,
            `sum(TotalRuminating)` = sum(TotalRuminating)/sumTotalIndividualsdf,
            `sum(TotalWalking)` = sum(TotalWalking)/sumTotalIndividualsdf,
            `sum(TotalDisplaying)` = sum(TotalDisplaying)/sumTotalIndividualsdf,
            `sum(TotalOther)` = sum(TotalOther)/sumTotalIndividualsdf,
            `sum(TotalActive)` = sum(TotalActive)/sumTotalIndividualsdf,
            `sum(TotalInactive)` = sum(TotalInactive)/sumTotalIndividualsdf)%>%
  ungroup()

probdf <- data.frame(Activity = c("Foraging", "Resting", "Ruminating",
                                "Walking", "Displaying", "Other",
                                "Active", "Inactive"),
                   probability = c(0.535363, 0.2667447,
                                   0.1473068,
                                   0.03606557,
                                   0.006088993,
                                   0.008430913,
                                   0.5859485,
                                   0.4140515))
#TP1 data
sumTotalIndividualstp1df <- hourlytp1df %>%
  summarise(`sum(TotalIndividuals)` = sum(TotalIndividuals))

percenthourlytp1df <- hourlytp1df %>%
  summarise(`sum(TotalForaging)` = sum(TotalForaging)/sumTotalIndividualstp1df,
            `sum(TotalResting)` = sum(TotalResting)/sumTotalIndividualstp1df,
            `sum(TotalRuminating)` = sum(TotalRuminating)/sumTotalIndividualstp1df,
            `sum(TotalWalking)` = sum(TotalWalking)/sumTotalIndividualstp1df,
            `sum(TotalDisplaying)` = sum(TotalDisplaying)/sumTotalIndividualstp1df,
            `sum(TotalOther)` = sum(TotalOther)/sumTotalIndividualstp1df,
            `sum(TotalActive)` = sum(TotalActive)/sumTotalIndividualstp1df,
            `sum(TotalInactive)` = sum(TotalInactive)/sumTotalIndividualstp1df)%>%
  ungroup()

probtp1df <- data.frame(Activity = c("Foraging", "Resting", "Ruminating",
                                  "Walking", "Displaying", "Other"),
                     probability = c(0.6037859,
                                     0.2278068,
                                     0.1279373,
                                     0.03198433,
                                     0.001305483,
                                     0.007180157))
#TP2 data
sumTotalIndividualstp2df <- hourlytp2df %>%
  summarise(`sum(TotalIndividuals)` = sum(TotalIndividuals))

percenthourlytp2df <- hourlytp2df %>%
  summarise(`sum(TotalForaging)` = sum(TotalForaging)/sumTotalIndividualstp2df,
            `sum(TotalResting)` = sum(TotalResting)/sumTotalIndividualstp2df,
            `sum(TotalRuminating)` = sum(TotalRuminating)/sumTotalIndividualstp2df,
            `sum(TotalWalking)` = sum(TotalWalking)/sumTotalIndividualstp2df,
            `sum(TotalDisplaying)` = sum(TotalDisplaying)/sumTotalIndividualstp2df,
            `sum(TotalOther)` = sum(TotalOther)/sumTotalIndividualstp2df,
            `sum(TotalActive)` = sum(TotalActive)/sumTotalIndividualstp2df,
            `sum(TotalInactive)` = sum(TotalInactive)/sumTotalIndividualstp2df)%>%
  ungroup()

probtp2df <- data.frame(Activity = c("Foraging", "Resting", "Ruminating",
                                  "Walking", "Displaying", "Other"),
                     probability = c(0.4970782,
                                     0.2885318,
                                     0.1581446,
                                     0.03834916,
                                     0.008765522,
                                     0.009130752))
#Female data
sumTotalIndividualsfemaledf <- hourlyfemaledf %>%
  summarise(`sum(TotalIndividuals)` = sum(TotalIndividuals))

percenthourlyfemaledf <- hourlyfemaledf %>%
  summarise(`sum(TotalForaging)` = sum(TotalForaging)/sumTotalIndividualsfemaledf,
            `sum(TotalResting)` = sum(TotalResting)/sumTotalIndividualsfemaledf,
            `sum(TotalRuminating)` = sum(TotalRuminating)/sumTotalIndividualsfemaledf,
            `sum(TotalWalking)` = sum(TotalWalking)/sumTotalIndividualsfemaledf,
            `sum(TotalDisplaying)` = sum(TotalDisplaying)/sumTotalIndividualsfemaledf,
            `sum(TotalOther)` = sum(TotalOther)/sumTotalIndividualsdf,
            `sum(TotalActive)` = sum(TotalActive)/sumTotalIndividualsdf,
            `sum(TotalInactive)` = sum(TotalInactive)/sumTotalIndividualsdf)%>%
  ungroup()

probfemaledf <- data.frame(Activity = c("Foraging", "Resting", "Ruminating",
                                        "Walking", "Displaying", "Other",
                                        "Active", "Inactive"),
                     probability = c(0.5458763,
                                     0.2340206,
                                     0.1639175,
                                     0.03298969,
                                     0.01340206,
                                     0.009793814,
                                     0.2735363,
                                     0.1807963))
#Male data
sumTotalIndividualsmaledf <- hourlymaledf %>%
  summarise(`sum(TotalIndividuals)` = sum(TotalIndividuals))

percenthourlymaledf <- hourlymaledf %>%
  summarise(`sum(TotalForaging)` = sum(TotalForaging)/sumTotalIndividualsmaledf,
            `sum(TotalResting)` = sum(TotalResting)/sumTotalIndividualsmaledf,
            `sum(TotalRuminating)` = sum(TotalRuminating)/sumTotalIndividualsmaledf,
            `sum(TotalWalking)` = sum(TotalWalking)/sumTotalIndividualsmaledf,
            `sum(TotalDisplaying)` = sum(TotalDisplaying)/sumTotalIndividualsmaledf,
            `sum(TotalOther)` = sum(TotalOther)/sumTotalIndividualsdf,
            `sum(TotalActive)` = sum(TotalActive)/sumTotalIndividualsdf,
            `sum(TotalInactive)` = sum(TotalInactive)/sumTotalIndividualsdf)%>%
  ungroup()

probmaledf <- data.frame(Activity = c("Foraging", "Resting", "Ruminating",
                                      "Walking", "Displaying", "Other",
                                      "Active", "Inactive"),
                     probability = c(0.4301221,
                                     0.3527815,
                                     0.1628223,
                                     0.04748982,
                                     0,
                                     0.006784261))
#Calf/yearling data
sumTotalIndividualsotherdf <- hourlyotherdf %>%
  summarise(`sum(TotalIndividuals)` = sum(TotalIndividuals))

percenthourlyotherdf <- hourlyotherdf %>%
  summarise(`sum(TotalForaging)` = sum(TotalForaging)/sumTotalIndividualsotherdf,
            `sum(TotalResting)` = sum(TotalResting)/sumTotalIndividualsotherdf,
            `sum(TotalRuminating)` = sum(TotalRuminating)/sumTotalIndividualsotherdf,
            `sum(TotalWalking)` = sum(TotalWalking)/sumTotalIndividualsotherdf,
            `sum(TotalDisplaying)` = sum(TotalDisplaying)/sumTotalIndividualsotherdf,
            `sum(TotalOther)` = sum(TotalOther)/sumTotalIndividualsdf,
            `sum(TotalActive)` = sum(TotalActive)/sumTotalIndividualsdf,
            `sum(TotalInactive)` = sum(TotalInactive)/sumTotalIndividualsdf)%>%
  ungroup()

probotherdf <- data.frame(Activity = c("Foraging", "Resting", "Ruminating",
                                       "Walking", "Displaying", "Other",
                                       "Active", "Inactive"),
                     probability = c(0.5712492,
                                     0.2667922,
                                     0.1198996,
                                     0.03452605,
                                     0,
                                     0.007532957))

##Grouping activity data----

FrExDa <- FrExDa %>% mutate(Løsning=recode(Løsning, A="Guro",B="Marie",C="Hanne",D="Linn",E="Anne",Ff="-Frida"),
                            Frass = recode(Frass, UF = "Uten frass", MF = "Med frass"))

df2 <- df2 %>% mutate(Sex=recode(Sex, 0="Female", 1="Male",99="Calf/yearling"))

# specify the order in which you want your new treatment names to appear in your graphs
loslevels <- c("Guro", "Marie", "Hanne", "Linn","Anne","Frida")
frasslevels <- c("Uten frass","Med frass")
FrExDa$Losning<-factor(FrExDa$Losning, levels = loslevels)
FrExDa$Frass<-factor(FrExDa$Frass, levels = frasslevels)



df %>%
  group_by(genre)

Activity = c("Foraging", "Resting", "Ruminating",
            "Walking", "Displaying", "Other")
Groups = c("Female", "Male", "Calf/yearling", "Total")

# Calculating mean weather ------------------------------------------------
#Mean weather + standard deviation (should be set to only include 19.03-05.05)
meanweather <- dfweather %>%
  summarize(meanSnowDepth = mean(SnowDepth, na.rm = TRUE), 
            sdSnowDepth = sd(SnowDepth, na.rm = TRUE),
            meanPrecipitation = mean(Precipitation, na.rm = TRUE),
            sdPrecipitation = sd(Precipitation, na.rm = TRUE),
            meanAirTemp = mean(AirTemp, na.rm = TRUE), 
            sdAirTemp = sd(AirTemp, na.rm = TRUE),
            meanMeanWindSpeed = mean(MeanWindSpeed, na.rm = TRUE), 
            sdMeanWindSpeed = sd(MeanWindSpeed, na.rm = TRUE))


# Plotting weather data ---------------------------------------------------
#Air temperature
lims <- as.POSIXct(strptime(c("19.03.2022", "05.05.2022"), 
                            format = "%d.%m.%Y"))

at <- ggplot(dfweather, mapping = aes(x=date_time, y=AirTemp)) +
  geom_line(color="#6bb0c7", size=0.8) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 13),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 0.25,
        plot.title = element_text(hjust = 0.5, size = 16)) +
  ggtitle("Air temperature (1 h)") +
  ylab("Temperature (°C)") +
  scale_x_datetime(limits = lims, date_breaks = "3 day", date_labels = '%d.%m', expand=c(0.06, 0))

ggsave(file="AirTemp.png", at, width=10, height=3, dpi=300)
 
#Precipitation
p <- ggplot(dfweather, mapping = aes(x=date_time, y=Precipitation)) +
  geom_line(color="#6bb0c7", size=0.8) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x=element_blank(),axis.title.y=element_text(size = 13),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 0.25,
        plot.title = element_text(hjust = 0.5, size = 16)) +
  ggtitle("Precipitation (1 h)") +
  ylab("Precipitation (mm)") +
  scale_x_datetime(limits = lims, date_breaks = "3 day", date_labels = '%d.%m', expand=c(0.06, 0))

ggsave(file="Precipitation.png", p, width=10, height=3, dpi=300)

#Snow depth
sd <- ggplot(dfweather, mapping = aes(x=date_time, y=SnowDepth)) +
  geom_line(color="#6bb0c7", size=0.8) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x=element_blank(),axis.title.y=element_text(size = 13),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 0.25,
        plot.title = element_text(hjust = 0.5, size = 16)) +
  ggtitle("Snow depth (1 h)") +
  ylab("Depth (cm)") +
  scale_x_datetime(limits = lims, date_breaks = "3 day", date_labels = '%d.%m', expand=c(0.06, 0))

ggsave(file="SnowDepth.png", sd, width=10, height=3, dpi=300)

#Wind speed
ws <- ggplot(dfweather, mapping = aes(x=date_time, y=MeanWindSpeed)) +
  geom_line(color="#6bb0c7", size=0.8) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x=element_blank(),axis.title.y=element_text(size = 13),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 0.25,
        plot.title = element_text(hjust = 0.5, size = 16)) +
  ggtitle("Mean wind speed (1 h)") +
  ylab("Wind speed (m/s)") +
  scale_x_datetime(limits = lims, date_breaks = "3 day", date_labels = '%d.%m', expand=c(0.06, 0))

ggsave(file="WindSpeed.png", ws, width=10, height=3, dpi=300)

# Plotting activity data --------------------------------------------------
##Percentage of time spent on each activity total----

#colorPalette <- c("#F0E442", "#7abbcf", "#1a588f", "#d67860", 
#                    "#d6954f", "#d67860")

#Overall data
act_t <- ggplot(data=probdf, aes(x= reorder(Activity, -probability), y=probability,
                              fill = Activity)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle("Total") +
  xlab(F) +
  ylab("Percentage of total time") +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.6))+
  scale_fill_manual("", 
                    breaks = c("Foraging", "Resting",
                               "Ruminating", "Walking",
                               "Displaying", "Other"),
                    values = c("#6bb0c7", "#659470", "#b36256",
                               "#d9d289", "#d67860", "#4b5e96"))
ggsave(file="act_t.png", act_t, width=4, height=3.1, dpi=300)

#Female data
act_f <- ggplot(data=probfemaledf, aes(x= reorder(Activity, -probability), y=probability,
                            fill = Activity)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle("Female") +
  xlab(F) +
  ylab("Percentage of total time") +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.6))+
  scale_fill_manual("", 
                    breaks = c("Foraging", "Resting",
                               "Ruminating", "Walking",
                               "Displaying", "Other"),
                    values = c("#6bb0c7", "#659470", "#b36256",
                               "#d9d289", "#d67860", "#4b5e96"))
ggsave(file="act_f.png", act_f, width=4, height=3.1, dpi=300)

#Male data
act_m <- ggplot(data=probmaledf, aes(x= reorder(Activity, -probability), y=probability,
                             fill = Activity)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle("Male") +
  xlab(F) +
  ylab("Percentage of total time") +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.6))+
  scale_fill_manual("", 
                    breaks = c("Foraging", "Resting",
                               "Ruminating", "Walking",
                               "Other"),
                    values = c("#6bb0c7", "#659470", "#b36256",
                               "#d9d289", "#4b5e96", "#d67860"))

ggsave(file="act_m.png", act_m, width=4, height=3.1, dpi=300)

#Other data
act_o <- ggplot(data=probotherdf, aes(x= reorder(Activity, -probability), y=probability,
                             fill = Activity)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle("Calf/yearling") +
  xlab(F) +
  ylab("Percentage of total time") +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.6))+
  scale_fill_manual("", 
                    breaks = c("Foraging", "Resting",
                               "Ruminating", "Walking",
                               "Other"),
                    values = c("#6bb0c7", "#659470", "#b36256",
                               "#d9d289", "#4b5e96", "#d67860"))

ggsave(file="act_o.png", act_o, width=4, height=3.1, dpi=300)

##Activity in March vs April/May----
#TP1
tp1 <- ggplot(data=probtp1df, aes(x= reorder(Activity, -probability), y=probability,
                        fill = Activity)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle("Late winter") +
  xlab(F) +
  ylab("Percentage of total time") +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.65))+
  scale_fill_manual("", 
                    breaks = c("Foraging", "Resting",
                               "Ruminating", "Walking",
                               "Displaying", "Other"),
                    values = c("#6bb0c7", "#659470", "#b36256",
                               "#d9d289", "#d67860", "#4b5e96"))

ggsave(file="activity_tp1.png", tp1, width=4, height=3.1, dpi=300)

#TP2
tp2 <- ggplot(data=probtp2df, aes(x= reorder(Activity, -probability), y=probability,
                        fill = Activity)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle("Polar day") +
  xlab(F) +
  ylab("Percentage of total time") +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.65))+
  scale_fill_manual("", 
                    breaks = c("Foraging", "Resting",
                               "Ruminating", "Walking",
                               "Displaying", "Other"),
                    values = c("#6bb0c7", "#659470", "#b36256",
                               "#d9d289", "#d67860", "#4b5e96"))

ggsave(file="activity_tp2.png", tp2, width=4, height=3.1, dpi=300)


##Percentage active/inactive----
#Overall data
ggplot(data=probdf, aes(x= reorder(Activity, -probability), y=probability,
                        fill = Activity)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle("Total") +
  xlab(F) +
  ylab("Percentage of total time") +
  scale_fill_manual("", 
                    breaks = c("Active", "Inactive"),
                    values = c("#6bb0c7", "#659470"))

#Female data

#Male data

#Other data

##Activity cycles/periodicity----
#Plotting data on one specific day, aiming to show the activity cycles in the reindeer.
#Best to pick a group (or two) that stay visible the entire time, don't go through mergers and were
#scanned for a long period of time (6 hours+).
#Showing more than one group for the same period would also say something about the
#synchrony between the groups.

ac <- ggplot(activitycycle, mapping = aes(x=date_time, y=Active)) +
  geom_line(color="#6bb0c7", size=0.8) +
  theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 13),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        aspect.ratio = 0.25,
        plot.title = element_text(hjust = 0.5, size = 16)) +
  #ggtitle("Air temperature (1 h)") +
        scale_x_datetime(breaks = scales::date_breaks("30 mins"), date_labels = "%H:%M")+
        ylab("% active")

ggsave(file="ActivityCycle.png", ac, width=10, height=3, dpi=300)


# Statistical analysis ----------------------------------------------------
##Multiple linear regression----
###Weather effect on activity----

#
cor.dat <- na.omit(hourlydf)
cor.test<-data.matrix(cor.dat[c("SnowDepth","Precipitation","AirTemp",
                                "MeanWindSpeed","TotalResting","TotalForaging","TotalRuminating")])

cor.coef <- cor(cor.test)
cor.coef

#Total foraging
m5 <- lm(TotalForaging~Precipitation+AirTemp
         +MeanWindSpeed, data=hourlydf)

summary(m5)

#Total resting
m6 <- lm(TotalResting~Precipitation+AirTemp
         +MeanWindSpeed, data=hourlydf)

summary(m6)

#Total ruminating
m7 <- lm(TotalRuminating~Precipitation+AirTemp
         +MeanWindSpeed, data=hourlydf)

summary(m7)





#Trying to get striped bars
ggplot(data=probdf, aes(fill=Condition, y=Level, x=Group, pattern = Condition, 
                pattern_type = Condition)) +
  geom_bar_pattern(position="dodge", stat="identity", pattern_fill = "black",
                   fill = "white", colour = "black", pattern_spacing = 0.01,
                   pattern_frequency = 5, pattern_angle = 45) +
  ggpubr::theme_pubr() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = 12)) +
  ggtitle("Total") +
  xlab(F) +
  ylab("Percentage of total time") +
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.6))+
  scale_fill_manual("", 
                    breaks = c("Foraging", "Resting",
                               "Ruminating", "Walking",
                               "Displaying", "Other"),
                    values = c("#6bb0c7", "#659470", "#b36256",
                               "#d9d289", "#d67860", "#4b5e96")) +
  scale_pattern_manual(values=c('stripe', 'none')) +
  scale_pattern_type_manual(values=c(NA, NA))

