# This script runs the statistical analyses with twitter followers as a proxy for sponsorship income

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

# Removing top earners and rankings

data <- data[which(data$Ranking_S > 11),]

# Creating male and female only data.frames

datam <- data[which(data$Gender == 'M'),]
dataf <- data[which(data$Gender == 'F'),]

datam <- datam[which(datam$Winnings < 21061920),]
dataf <- dataf[which(dataf$Winnings < 20011888),]

data <- rbind(datam,dataf)

# Creating male and female only data.frames

datam <- data[which(data$Gender == 'M'),]
dataf <- data[which(data$Gender == 'F'),]

# Running the (logit) models for all players

# Model 1 is just regressing gender on participation

mod1 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = data)

# Model 2 is gender + age

mod2 <- glm(Competed ~ factor(Gender) + Age, family = binomial(link = logit), data = data)

# Model 3 is gender + age + career earnings

mod3 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1), family = binomial(link = logit), data = data)

# Model 4 is gender + age + YTD earnings

mod4 <- glm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), family = binomial(link = logit), data = data)

# Model 5 is gender + age + career earnings + YTD earnings

mod5 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = data)

# Model 6 is Model 5 + remaining controls

mod6 <- glm(Competed ~ factor(Gender) + Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + factor(Tournament)*factor(Country) + Prev_Tourn_Comp,
            family = binomial(link = logit), data = data)

# Add Followers

mod7 <- glm(Competed ~ factor(Gender) + Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + factor(Tournament)*factor(Country) + Prev_Tourn_Comp + log(Followers),
            family = binomial(link = logit), data = data)

# Running the (logit) models for singles players who were not 'substitutes' only

# Subsetting data for singles players who were not 'substitutes' only

subdata <- data[which(data$Lucky == 0),]
subdatam <- datam[which(datam$Lucky == 0),]
subdataf <- dataf[which(dataf$Lucky == 0),]

# Subsetting to remove outliers for twitter followers

#upper <- quantile(subdata$Followers, c(.98), na.rm = TRUE)
#subdatat <- data[which(data$Followers < upper),]
subdatat <- data

# Model 1 is just regressing gender on participation

submod1 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata)

# Model 2 is gender + age

submod2 <- glm(Competed ~ factor(Gender) + Age, family = binomial(link = logit), data = subdata)

# Model 3 is gender + age + career earnings

submod3 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1), family = binomial(link = logit), data = subdata)

# Model 4 is gender + age + YTD earnings

submod4 <- glm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata)

# Model 5 is gender + age + career earnings + YTD earnings

submod5 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1),
               family = binomial(link = logit), data = subdata)

# Model 6 is Model 5 + controls

submod6 <- glm(Competed ~ factor(Gender) + Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
               + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
               + factor(Tournament)+factor(Country) + Prev_Tourn_Comp,
               family = binomial(link = logit), data = subdata)

# Add Followers

submod7 <- glm(Competed ~ factor(Gender) + Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
               + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
               + factor(Tournament)*factor(Country) + Prev_Tourn_Comp + log(Followers),
               family = binomial(link = logit), data = subdata)

# Generating heteroskedasticity robust standard errors

cov <- vcovHC(mod1, type = 'HC0')
rse1 <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod2, type = 'HC0')
rse2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod3, type = 'HC0')
rse3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod4, type = 'HC0')
rse4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod5, type = 'HC0')
rse5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod6, type = 'HC0')
rse6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod7, type = 'HC0')
rse7 <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod1, type = 'HC0')
subrse1 <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod2, type = 'HC0')
subrse2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod3, type = 'HC0')
subrse3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod4, type = 'HC0')
subrse4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod5, type = 'HC0')
subrse5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod6, type = 'HC0')
subrse6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod7, type = 'HC0')
subrse7 <- sqrt(abs(diag(cov)))

# Viewing regression results and saving to file

write.csv(stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7, type = 'text',
                    se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7,
                    se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers_tex.txt', sep = ''), row.names = FALSE)

stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7, type = 'text',
          se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7), omit = c('Country', 'Tournament'))

# Repeat all regressions for women and men only

mod2m <- glm(Competed ~ Age, family = binomial(link = logit), data = datam)

mod3m <- glm(Competed ~ Age + log(Winnings + 1), family = binomial(link = logit), data = datam)

mod4m <- glm(Competed ~ Age + log(Winnings_20 + 1), family = binomial(link = logit), data = datam)

mod5m <- glm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = datam)

mod6m <- glm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament)*factor(Country) + Prev_Tourn_Comp
             + factor(Country), family = binomial(link = logit), data = subdatam)

mod7m <- glm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament)*factor(Country) + Prev_Tourn_Comp + log(Followers)
             + factor(Country), family = binomial(link = logit), data = subdatam)

mod2f <- glm(Competed ~ Age, family = binomial(link = logit), data = dataf)

mod3f <- glm(Competed ~ Age + log(Winnings + 1), family = binomial(link = logit), data = dataf)

mod4f <- glm(Competed ~ Age + log(Winnings_20 + 1), family = binomial(link = logit), data = dataf)

mod5f <- glm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = dataf)

mod6f <- glm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament)*factor(Country) + Prev_Tourn_Comp
             + factor(Country), family = binomial(link = logit), data = subdataf)

mod7f <- glm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament)*factor(Country) + Prev_Tourn_Comp + log(Followers)
             + factor(Country), family = binomial(link = logit), data = subdataf)

submod2m <- glm(Competed ~ Age, family = binomial(link = logit), data = subdatam)

submod3m <- glm(Competed ~ Age + log(Winnings + 1), family = binomial(link = logit), data = subdatam)

submod4m <- glm(Competed ~ Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdatam)

submod5m <- glm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdatam)

submod6m <- glm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
                + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
                + factor(Tournament)+factor(Country) + Prev_Tourn_Comp + factor(Country),
                family = binomial(link = logit), data = subdatam)

submod7m <- glm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
                + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
                + factor(Tournament)+factor(Country) + Prev_Tourn_Comp + factor(Country) + log(Followers),
                family = binomial(link = logit), data = subdatam)

submod2f <- glm(Competed ~ Age, family = binomial(link = logit), data = subdataf)

submod3f <- glm(Competed ~ Age + log(Winnings + 1), family = binomial(link = logit), data = subdataf)

submod4f <- glm(Competed ~ Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdataf)

submod5f <- glm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdataf)

submod6f <- glm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
                + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
                + factor(Tournament)+factor(Country) + Prev_Tourn_Comp + factor(Country),
                family = binomial(link = logit), data = subdataf)

submod7f <- glm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
                + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
                + factor(Tournament)+factor(Country) + Prev_Tourn_Comp + factor(Country) + log(Followers),
                family = binomial(link = logit), data = subdataf)

# Generating  heteroskedasticity robust standard errors

cov <- vcovHC(mod2m, type = 'HC0')
rse2m <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod3m, type = 'HC0')
rse3m <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod4m, type = 'HC0')
rse4m <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod5m, type = 'HC0')
rse5m <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod6m, type = 'HC0')
rse6m <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod7m, type = 'HC0')
rse7m <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod2f, type = 'HC0')
rse2f <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod3f, type = 'HC0')
rse3f <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod4f, type = 'HC0')
rse4f <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod5f, type = 'HC0')
rse5f <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod6f, type = 'HC0')
rse6f <- sqrt(abs(diag(cov)))

cov <- vcovHC(mod7f, type = 'HC0')
rse7f <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod2m, type = 'HC0')
subrse2m <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod3m, type = 'HC0')
subrse3m <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod4m, type = 'HC0')
subrse4m <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod5m, type = 'HC0')
subrse5m <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod6m, type = 'HC0')
subrse6m <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod7m, type = 'HC0')
subrse7m <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod2f, type = 'HC0')
subrse2f <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod3f, type = 'HC0')
subrse3f <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod4f, type = 'HC0')
subrse4f <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod5f, type = 'HC0')
subrse5f <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod6f, type = 'HC0')
subrse6f <- sqrt(abs(diag(cov)))

cov <- vcovHC(submod7f, type = 'HC0')
subrse7f <- sqrt(abs(diag(cov)))

# Writing results to file

write.csv(stargazer(submod2m, submod3m, submod4m, submod5m, submod6m, submod7m, type = 'text',
                    se = list(subrse2m, subrse3m, subrse4m, subrse5m, subrse6m, subrse7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers_M.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(submod2m, submod3m, submod4m, submod5m, submod6m, submod7m,
                    se = list(subrse2m, subrse3m, subrse4m, subrse5m, subrse6m, subrse7m), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers_M_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(submod2f, submod3f, submod4f, submod5f, submod6f, submod7f, type = 'text',
                    se = list(subrse2f, subrse3f, subrse4f, subrse5f, subrse6f, subrse7f), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers_F.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(submod2f, submod3f, submod4f, submod5f, submod6f, submod7f,                    
                    se = list(subrse2f, subrse3f, subrse4f, subrse5f, subrse6f, subrse7f), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers_F_tex.txt', sep = ''), row.names = FALSE)

stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7, type = 'text',
          se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7), omit = c('Country', 'Tournament'))

stargazer(submod2m, submod3m, submod4m, submod5m, submod6m, submod7m, type = 'text',
          se = list(subrse2m, subrse3m, subrse4m, subrse5m, subrse6m, submrsem), omit = c('Country', 'Tournament'))

stargazer(submod2f, submod3f, submod4f, submod5f, submod6f, submod7f, type = 'text',
          se = list(subrse2f, subrse3f, subrse4f, subrse5f, subrse6f, subrse7f), omit = c('Country', 'Tournament'))

# Summary statistics for data by gender after removing those who effectively did not make a choice

stargazer(subdata, summary.stat = c('mean', 'sd', 'min', 'max'))
stargazer(subdataf, summary.stat = c('mean', 'sd', 'min', 'max'))
stargazer(subdatam, summary.stat = c('mean', 'sd', 'min', 'max'))

stargazer(subdata, summary.stat = c('mean', 'sd', 'min', 'max'), type = 'text')
stargazer(subdataf, summary.stat = c('mean', 'sd', 'min', 'max'), type = 'text')
stargazer(subdatam, summary.stat = c('mean', 'sd', 'min', 'max'), type = 'text')

# Average marginal effects from the logistic models

mar1 <- margins(submod1)
mar2 <- margins(submod2)
mar3 <- margins(submod3)
mar4 <- margins(submod4)
mar5 <- margins(submod5)
mar6 <- margins(submod6)

mar2f <- margins(submod2f)
mar3f <- margins(submod3f)
mar4f <- margins(submod4f)
mar5f <- margins(submod5f)
mar6f <- margins(submod6f)

mar2m <- margins(submod2m)
mar3m <- margins(submod3m)
mar4m <- margins(submod4m)
mar5m <- margins(submod5m)
mar6m <- margins(submod6m)

# t-statistics

t1 <- submod1$coefficients / subrse1
t2 <- submod2$coefficients / subrse2
t3 <- submod3$coefficients / subrse3
t4 <- submod4$coefficients / subrse4
t5 <- submod5$coefficients / subrse5
t6 <- submod6$coefficients / subrse6

t2f <- submod2f$coefficients / subrse2f
t3f <- submod3f$coefficients / subrse3f
t4f <- submod4f$coefficients / subrse4f
t5f <- submod5f$coefficients / subrse5f
t6f <- submod6f$coefficients / subrse6f

t2m <- submod2m$coefficients / subrse2m
t3m <- submod3m$coefficients / subrse3m
t4m <- submod4m$coefficients / subrse4m
t5m <- submod5m$coefficients / subrse5m
t6m <- submod6m$coefficients / subrse6m

# OLS for reviewer

# Model 1 is just regressing gender on participation

omod1 <- lm(Competed ~ factor(Gender), data = subdata)

# Model 2 is gender + age

omod2 <- lm(Competed ~ factor(Gender) + Age, data = subdata)

# Model 3 is gender + age + career earnings

omod3 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1), data = subdata)

# Model 4 is gender + age + YTD earnings

omod4 <- lm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), data = subdata)

# Model 5 is gender + age + career earnings + YTD earnings

omod5 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1), data = subdata)

# Model 6 is Model 5 + controls

omod6 <- lm(Competed ~ factor(Gender) + Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + factor(Tournament)*factor(Country) + Prev_Tourn_Comp,
            data = subdatat)

omod7 <- lm(Competed ~ factor(Gender) + Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + factor(Tournament)*factor(Country) + Prev_Tourn_Comp + log(Followers),
            data = subdatat)

cov <- vcovHC(omod1, type = 'HC0')
o1 <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod2, type = 'HC0')
o2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod3, type = 'HC0')
o3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod4, type = 'HC0')
o4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod5, type = 'HC0')
o5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod6, type = 'HC0')
o6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod7, type = 'HC0')
o7 <- sqrt(abs(diag(cov)))

omod2f <- lm(Competed ~ Age, data = subdataf)

omod3f <- lm(Competed ~ Age + log(Winnings + 1), data = subdataf)

omod4f <- lm(Competed ~ Age + log(Winnings_20 + 1), data = subdataf)

omod5f <- lm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), data = subdataf)

omod6f <- lm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament)+factor(Country) + Prev_Tourn_Comp,
             data = subdataf)

omod7f <- lm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament)+factor(Country) + Prev_Tourn_Comp + log(Followers),
             data = subdataf)

cov <- vcovHC(omod2f, type = 'HC0')
o2f <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod3f, type = 'HC0')
o3f <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod4f, type = 'HC0')
o4f <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod5f, type = 'HC0')
o5f <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod6f, type = 'HC0')
o6f <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod7f, type = 'HC0')
o7f <- sqrt(abs(diag(cov)))

omod2m <- lm(Competed ~ Age, data = subdatam)

omod3m <- lm(Competed ~ Age + log(Winnings + 1), data = subdatam)

omod4m <- lm(Competed ~ Age + log(Winnings_20 + 1), data = subdatam)

omod5m <- lm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), data = subdatam)

omod6m <- lm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament)+factor(Country) + Prev_Tourn_Comp,
             data = subdatam)

omod7m <- lm(Competed ~ Age + Age*Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament)+factor(Country) + Prev_Tourn_Comp + log(Followers),
             data = subdatam)

cov <- vcovHC(omod2m, type = 'HC0')
o2m <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod3m, type = 'HC0')
o3m <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod4m, type = 'HC0')
o4m <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod5m, type = 'HC0')
o5m <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod6m, type = 'HC0')
o6m <- sqrt(abs(diag(cov)))

cov <- vcovHC(omod7m, type = 'HC0')
o7m <- sqrt(abs(diag(cov)))

write.csv(stargazer(omod1, omod2, omod3, omod4, omod5, omod6, omod7,
                    se = list(o1, o2, o3, o4, o5, o6, o7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers_OLS_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(omod2m, omod3m, omod4m, omod5m, omod6m, omod7m,
                    se = list(o2m, o3m, o4m, o5m, o6m, o7m), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers_M_OLS_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(omod2f, omod3f, omod4f, omod5f, omod6f, omod7f,
                    se = list(o2f, o3f, o4f, o5f, o6f, o7f), omit = c('Country', 'Tournament')),
          paste(directory, 'results/main/SSS_results_lucky_full_season_w_followers_F_OLS_tex.txt', sep = ''), row.names = FALSE)

# Creating fixed effects for AgeX groups

AgeX <- c()

for (i in 1:dim(subdata)[1]) {
  
  if (as.numeric(data$Age[i]) < 20) {
    
    AgeX <- c(AgeX, '10s')
    
  } else if (as.numeric(data$Age[i]) > 29) {
    
    AgeX <- c(AgeX, '30s')
    
  } else {
    
    AgeX <- c(AgeX, '20s')
    
  }
  
}

subdata$AgeX <- AgeX

amod2 <- lm(Competed ~ factor(Gender), data = subdata[which(subdata$AgeX == '10s'),])

amod3 <- lm(Competed ~ factor(Gender) + log(Winnings + 1), data = subdata[which(subdata$AgeX == '10s'),])

amod4 <- lm(Competed ~ factor(Gender) + log(Winnings_20 + 1), data = subdata[which(subdata$AgeX == '10s'),])

amod5 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1), data = subdata[which(subdata$AgeX == '10s'),])

amod6 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + factor(Tournament) + Prev_Tourn_Comp,
            data = subdata[which(subdata$AgeX == '10s'),])

amod7 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + factor(Tournament) + Prev_Tourn_Comp + log(Followers),
            data = subdata[which(subdata$AgeX == '10s'),])

cov <- vcovHC(amod2, type = 'HC0')
a2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod3, type = 'HC0')
a3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod4, type = 'HC0')
a4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod5, type = 'HC0')
a5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod6, type = 'HC0')
a6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod7, type = 'HC0')
a7 <- sqrt(abs(diag(cov)))

a2mod2 <- lm(Competed ~ factor(Gender), data = subdata[which(subdata$AgeX == '20s'),])

a2mod3 <- lm(Competed ~ factor(Gender) + log(Winnings + 1), data = subdata[which(subdata$AgeX == '20s'),])

a2mod4 <- lm(Competed ~ factor(Gender) + log(Winnings_20 + 1), data = subdata[which(subdata$AgeX == '20s'),])

a2mod5 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1), data = subdata[which(subdata$AgeX == '20s'),])

a2mod6 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament) + Prev_Tourn_Comp,
             data = subdata[which(subdata$AgeX == '20s'),])

a2mod7 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament) + Prev_Tourn_Comp + log(Followers),
             data = subdata[which(subdata$AgeX == '20s'),])

cov <- vcovHC(a2mod2, type = 'HC0')
a22 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod3, type = 'HC0')
a23 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod4, type = 'HC0')
a24 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod5, type = 'HC0')
a25 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod6, type = 'HC0')
a26 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod7, type = 'HC0')
a27 <- sqrt(abs(diag(cov)))

a3mod2 <- lm(Competed ~ factor(Gender), data = subdata[which(subdata$AgeX == '30s'),])

a3mod3 <- lm(Competed ~ factor(Gender) + log(Winnings + 1), data = subdata[which(subdata$AgeX == '30s'),])

a3mod4 <- lm(Competed ~ factor(Gender) + log(Winnings_20 + 1), data = subdata[which(subdata$AgeX == '30s'),])

a3mod5 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1), data = subdata[which(subdata$AgeX == '30s'),])

a3mod6 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament) + Prev_Tourn_Comp,
             data = subdata[which(subdata$AgeX == '30s'),])

a3mod7 <- lm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament) + Prev_Tourn_Comp + log(Followers),
             data = subdata[which(subdata$AgeX == '30s'),])

cov <- vcovHC(a3mod2, type = 'HC0')
a32 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod3, type = 'HC0')
a33 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod4, type = 'HC0')
a34 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod5, type = 'HC0')
a35 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod6, type = 'HC0')
a36 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod7, type = 'HC0')
a37 <- sqrt(abs(diag(cov)))

write.csv(stargazer(amod2, amod3, amod4, amod5, amod6, amod7,
                    se = list(a2, a3, a4, a5, a6, a7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/groups/SSS_results_lucky_full_season_w_followers_10s_OLS_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(a2mod2, a2mod3, a2mod4, a2mod5, a2mod6, a2mod7,
                    se = list(a22, a23, a24, a25, a26, a27), omit = c('Country', 'Tournament')),
          paste(directory, 'results/groups/SSS_results_lucky_full_season_w_followers_20s_OLS_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(a3mod2, a3mod3, a3mod4, a3mod5, a3mod6, a3mod7,
                    se = list(a32, a33, a34, a35, a36,a37), omit = c('Country', 'Tournament')),
          paste(directory, 'results/groups/SSS_results_lucky_full_season_w_followers_30s_OLS_tex.txt', sep = ''), row.names = FALSE)

umod2 <- lm(Competed ~ factor(Gender), data = subdata[which(subdata$Tournament == 'US'),])

umod3 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1), data = subdata[which(subdata$Tournament == 'US'),])

umod4 <- lm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), data = subdata[which(subdata$Tournament == 'US'),])

umod5 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1), data = subdata[which(subdata$Tournament == 'US'),])

umod6 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + Prev_Tourn_Comp, data = subdata[which(subdata$Tournament == 'US'),])

umod7 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + Prev_Tourn_Comp + log(Followers), data = subdata[which(subdata$Tournament == 'US'),])

cov <- vcovHC(umod2, type = 'HC0')
u2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod3, type = 'HC0')
u3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod4, type = 'HC0')
u4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod5, type = 'HC0')
u5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod6, type = 'HC0')
u6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod7, type = 'HC0')
u7 <- sqrt(abs(diag(cov)))

imod2 <- lm(Competed ~ factor(Gender), data = subdata[which(subdata$Tournament == 'Italian'),])

imod3 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1), data = subdata[which(subdata$Tournament == 'Italian'),])

imod4 <- lm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), data = subdata[which(subdata$Tournament == 'Italian'),])

imod5 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1), data = subdata[which(subdata$Tournament == 'Italian'),])

imod6 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + Prev_Tourn_Comp, data = subdata[which(subdata$Tournament == 'Italian'),])

imod7 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + Prev_Tourn_Comp + log(Followers), data = subdata[which(subdata$Tournament == 'Italian'),])

cov <- vcovHC(imod2, type = 'HC0')
i2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod3, type = 'HC0')
i3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod4, type = 'HC0')
i4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod5, type = 'HC0')
i5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod6, type = 'HC0')
i6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod7, type = 'HC0')
i7 <- sqrt(abs(diag(cov)))

fmod2 <- lm(Competed ~ factor(Gender), data = subdata[which(subdata$Tournament == 'French'),])

fmod3 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1), data = subdata[which(subdata$Tournament == 'French'),])

fmod4 <- lm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), data = subdata[which(subdata$Tournament == 'French'),])

fmod5 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1), data = subdata[which(subdata$Tournament == 'French'),])

fmod6 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + Prev_Tourn_Comp, data = subdata[which(subdata$Tournament == 'French'),])

fmod7 <- lm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
            + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
            + Prev_Tourn_Comp + log(Followers), data = subdata[which(subdata$Tournament == 'French'),])

cov <- vcovHC(fmod2, type = 'HC0')
f2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod3, type = 'HC0')
f3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod4, type = 'HC0')
f4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod5, type = 'HC0')
f5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod6, type = 'HC0')
f6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod7, type = 'HC0')
f7 <- sqrt(abs(diag(cov)))

write.csv(stargazer(umod2, umod3, umod4, umod5, umod6, umod7,
                    se = list(u2, u3, u4, u5, u6, u7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/us/SSS_results_lucky_full_season_w_followers_OLS_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(imod2, imod3, imod4, imod5, imod6, imod7,
                    se = list(i2, i3, i4, i5, i6, i7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/italian/SSS_results_lucky_full_season_w_followers_OLS_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(fmod2, fmod3, fmod4, fmod5, fmod6, fmod7,
                    se = list(f2, f3, f4, f5, f6, f7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/french/SSS_results_lucky_full_season_w_followers_OLS_tex.txt', sep = ''), row.names = FALSE)

# Now doing the logits for the last two sets of models

amod2 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '10s'),])

amod3 <- glm(Competed ~ factor(Gender) + log(Winnings + 1), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '10s'),])

amod4 <- glm(Competed ~ factor(Gender) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '10s'),])

amod5 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1),
             family = binomial(link = logit), data = subdata[which(subdata$AgeX == '10s'),])

amod6 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament) + Prev_Tourn_Comp,
             family = binomial(link = logit), data = subdata[which(subdata$AgeX == '10s'),])

amod7 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + factor(Tournament) + Prev_Tourn_Comp + log(Followers),
             family = binomial(link = logit), data = subdata[which(subdata$AgeX == '10s'),])

cov <- vcovHC(amod2, type = 'HC0')
a2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod3, type = 'HC0')
a3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod4, type = 'HC0')
a4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod5, type = 'HC0')
a5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod6, type = 'HC0')
a6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(amod7, type = 'HC0')
a7 <- sqrt(abs(diag(cov)))

a2mod2 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '20s'),])

a2mod3 <- glm(Competed ~ factor(Gender) + log(Winnings + 1), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '20s'),])

a2mod4 <- glm(Competed ~ factor(Gender) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '20s'),])

a2mod5 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1),
              family = binomial(link = logit), data = subdata[which(subdata$AgeX == '20s'),])

a2mod6 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
              + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
              + factor(Tournament) + Prev_Tourn_Comp,
              family = binomial(link = logit), data = subdata[which(subdata$AgeX == '20s'),])

a2mod7 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
              + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
              + factor(Tournament) + Prev_Tourn_Comp + log(Followers),
              family = binomial(link = logit), data = subdata[which(subdata$AgeX == '20s'),])

cov <- vcovHC(a2mod2, type = 'HC0')
a22 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod3, type = 'HC0')
a23 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod4, type = 'HC0')
a24 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod5, type = 'HC0')
a25 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod6, type = 'HC0')
a26 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a2mod7, type = 'HC0')
a27 <- sqrt(abs(diag(cov)))

a3mod2 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '30s'),])

a3mod3 <- glm(Competed ~ factor(Gender) + log(Winnings + 1), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '30s'),])

a3mod4 <- glm(Competed ~ factor(Gender) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata[which(subdata$AgeX == '30s'),])

a3mod5 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1),
              family = binomial(link = logit), data = subdata[which(subdata$AgeX == '30s'),])

a3mod6 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
              + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
              + factor(Tournament) + Prev_Tourn_Comp,
              family = binomial(link = logit), data = subdata[which(subdata$AgeX == '30s'),])

a3mod7 <- glm(Competed ~ factor(Gender) + log(Winnings + 1) + log(Winnings_20 + 1)
              + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
              + factor(Tournament) + Prev_Tourn_Comp + log(Followers),
              family = binomial(link = logit), data = subdata[which(subdata$AgeX == '30s'),])


cov <- vcovHC(a3mod2, type = 'HC0')
a32 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod3, type = 'HC0')
a33 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod4, type = 'HC0')
a34 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod5, type = 'HC0')
a35 <- sqrt(abs(diag(cov)))

cov <- vcovHC(a3mod6, type = 'HC0')
a36 <- sqrt(abs(diag(cov)))

write.csv(stargazer(amod2, amod3, amod4, amod5, amod6, amod7,
                    se = list(a2, a3, a4, a5, a6, a7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/groups/SSS_results_lucky_full_season_w_followers_10s_LOGIT_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(a2mod2, a2mod3, a2mod4, a2mod5, a2mod6, a2mod7,
                    se = list(a22, a23, a24, a25, a26, a27), omit = c('Country', 'Tournament')),
          paste(directory, 'results/groups/SSS_results_lucky_full_season_w_followers_20s_LOGIT_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(a3mod2, a3mod3, a3mod4, a3mod5, a3mod6, a3mod7,
                    se = list(a32, a33, a34, a35, a36, a37), omit = c('Country', 'Tournament')),
          paste(directory, 'results/groups/SSS_results_lucky_full_season_w_followers_30s_LOGIT_tex.txt', sep = ''), row.names = FALSE)

umod2 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'US'),])

umod3 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'US'),])

umod4 <- glm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'US'),])

umod5 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1),
             family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'US'),])

umod6 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'US'),])

umod7 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + Prev_Tourn_Comp + log(Followers), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'US'),])

cov <- vcovHC(umod2, type = 'HC0')
u2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod3, type = 'HC0')
u3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod4, type = 'HC0')
u4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod5, type = 'HC0')
u5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod6, type = 'HC0')
u6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(umod7, type = 'HC0')
u7 <- sqrt(abs(diag(cov)))

imod2 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'Italian'),])

imod3 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'Italian'),])

imod4 <- glm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'Italian'),])

imod5 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1),
             family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'Italian'),])

imod6 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'Italian'),])

imod7 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + Prev_Tourn_Comp + log(Followers), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'Italian'),])

cov <- vcovHC(imod2, type = 'HC0')
i2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod3, type = 'HC0')
i3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod4, type = 'HC0')
i4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod5, type = 'HC0')
i5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod6, type = 'HC0')
i6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(imod7, type = 'HC0')
i7 <- sqrt(abs(diag(cov)))

fmod2 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'French'),])

fmod3 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'French'),])

fmod4 <- glm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'French'),])

fmod5 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1),
             family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'French'),])

fmod6 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'French'),])

fmod7 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1)
             + Doubles + Qualifier + Ranking_S + Titles + Majors_Bi + Current_Bi
             + Prev_Tourn_Comp + log(Followers), family = binomial(link = logit), data = subdata[which(subdata$Tournament == 'French'),])

cov <- vcovHC(fmod2, type = 'HC0')
f2 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod3, type = 'HC0')
f3 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod4, type = 'HC0')
f4 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod5, type = 'HC0')
f5 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod6, type = 'HC0')
f6 <- sqrt(abs(diag(cov)))

cov <- vcovHC(fmod7, type = 'HC0')
f7 <- sqrt(abs(diag(cov)))

write.csv(stargazer(umod2, umod3, umod4, umod5, umod6, umod7,
                    se = list(u2, u3, u4, u5, u6, u7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/us/SSS_results_lucky_full_season_w_followers_LOGIT_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(imod2, imod3, imod4, imod5, imod6, imod7,
                    se = list(i2, i3, i4, i5, i6, i7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/italian/SSS_results_lucky_full_season_w_followers_LOGIT_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(fmod2, fmod3, fmod4, fmod5, fmod6, fmod7,
                    se = list(f2, f3, f4, f5, f6, f7), omit = c('Country', 'Tournament')),
          paste(directory, 'results/french/SSS_results_lucky_full_season_w_followers_LOGIT_tex.txt', sep = ''), row.names = FALSE)

# Average marginal effects from the logistic models

mar1 <- margins(submod1)
mar2 <- margins(submod2)
mar3 <- margins(submod3)
mar4 <- margins(submod4)
mar5 <- margins(submod5)
mar6 <- margins(submod6)
mar7 <- margins(submod7)

mar2f <- margins(submod2f)
mar3f <- margins(submod3f)
mar4f <- margins(submod4f)
mar5f <- margins(submod5f)
mar6f <- margins(submod6f)
mar7f <- margins(submod7f)

mar2m <- margins(submod2m)
mar3m <- margins(submod3m)
mar4m <- margins(submod4m)
mar5m <- margins(submod5m)
mar6m <- margins(submod6m)
mar7m <- margins(submod7m)

mar2a <- margins(amod2)
mar3a <- margins(amod3)
mar4a <- margins(amod4)
mar5a <- margins(amod5)
mar6a <- margins(amod6)
mar7a <- margins(amod7)

mar2a2 <- margins(a2mod2)
mar3a2 <- margins(a2mod3)
mar4a2 <- margins(a2mod4)
mar5a2 <- margins(a2mod5)
mar6a2 <- margins(a2mod6)
mar7a2 <- margins(a2mod7)

mar2a3 <- margins(a3mod2)
mar3a3 <- margins(a3mod3)
mar4a3 <- margins(a3mod4)
mar5a3 <- margins(a3mod5)
mar6a3 <- margins(a3mod6)
mar7a3 <- margins(a3mod7)

mar2u <- margins(umod2)
mar3u <- margins(umod3)
mar4u <- margins(umod4)
mar5u <- margins(umod5)
mar6u <- margins(umod6)
mar7u <- margins(umod7)

mar2i <- margins(imod2)
mar3i <- margins(imod3)
mar4i <- margins(imod4)
mar5i <- margins(imod5)
mar6i <- margins(imod6)
mar7i <- margins(imod7)

mar2f <- margins(fmod2)
mar3f <- margins(fmod3)
mar4f <- margins(fmod4)
mar5f <- margins(fmod5)
mar6f <- margins(fmod6)
mar7f <- margins(fmod7)

