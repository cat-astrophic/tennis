# This script makes (hopefully convincing) some plots

# Loading libraries

library(sandwich)
library(stargazer)
library(margins)
library(ggplot2)

# Reading in the data

us <- read.csv(paste(directory, 'us.csv', sep = ''), fileEncoding = 'UTF-8-BOM')
italian <- read.csv(paste(directory, 'italian.csv', sep = ''), fileEncoding = 'UTF-8-BOM')
french <- read.csv(paste(directory, 'french.csv', sep = ''), fileEncoding = 'UTF-8-BOM')

# Renaming some columns to perform a union

# NEED TO RENAME Current, etc., to "Current_Open"

names(us)[names(us) == 'US_Open'] <- 'Current'
names(us)[names(us) == 'US_Open_Bi'] <- 'Current_Bi'

names(italian)[names(italian) == 'Italian_Open'] <- 'Current'
names(italian)[names(italian) == 'Italian_Open_Bi'] <- 'Current_Bi'

names(french)[names(french) == 'French_Open'] <- 'Current'
names(french)[names(french) == 'French_Open_Bi'] <- 'Current_Bi'

# Adding a Tournament indicator for use as a fixed effect

ust <- rep('US', dim(us)[1])
itt <- rep('Italian', dim(italian)[1])
frt <- rep('French', dim(french)[1])

us$Tournament <- ust
italian$Tournament <- itt
french$Tournament <- frt

# Adding 7-day average new covid cases per nation on the start date of the event (data from JHU)

usc <- rep(41600, dim(us)[1])
itc <- rep(1423, dim(italian)[1])
frc <- rep(10116, dim(french)[1])

us$Start.Cases <- usc
italian$Start.Cases <- itc
french$Start.Cases <- frc

# Unioning the data

data <- rbind(us,italian,french)

# Adding an Age^2 variable to the data set

data$Age2 <- data$Age*data$Age

# Dropping individuals who did not choose whether or not to participate, i.e., those who had COVID, an injury, or were suspended

data <- data[which(data$COVID == 0),]
data <- data[which(data$Injury == 0),]
data <- data[which(data$Suspension == 0),]

# Creating male and female only data.frames

datam <- data[which(data$Gender == 'M'),]
dataf <- data[which(data$Gender == 'F'),]

# Subsetting for data with follower counts

data <- data[which(data$Followers > 0),]

# Making plots

Gender <- data$Gender

plotmod <- lm(log(Followers) ~ log(Winnings), data = data)
qplot(log(data$Winnings), log(data$Followers), col = Gender, xlab = 'Log(Winnings)', ylab = 'Log(Followers)',
      title = 'Twitter Followers v. Career Winnings', grid = FALSE) + 
      theme_classic() + geom_point() + geom_smooth(method = 'lm')

plotmod <- lm(log(Followers) ~ log(data$Titles+1), data = data)
qplot(log(data$Titles+1), log(data$Followers), col = Gender, xlab = 'Log(Titles)', ylab = 'Log(Followers)',
      title = 'Twitter Followers v. Career Titles', grid = FALSE) + 
  theme_classic() + geom_point() + geom_smooth(method = 'lm')

plotmod <- lm(log(Followers) ~ log(Ranking_S), data = data)
qplot(log(data$Ranking_S), log(data$Followers), col = Gender, xlab = 'Log(Ranking)', ylab = 'Log(Followers)',
      title = 'Twitter Followers v. Singles Ranking', grid = FALSE) + 
  theme_classic() + geom_point() + geom_smooth(method = 'lm')

