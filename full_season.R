# This script runs the statistical analyses for a paper on athletes, gender, and risk for all tournaments together

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

# Unioning the data

data <- rbind(us,italian,french)

# Dropping individuals who did not choose whether or not to participate, i.e., those who had COVID, an injury, or were suspended

data <- data[which(data$COVID == 0),]
data <- data[which(data$Injury == 0),]
data <- data[which(data$Suspension == 0),]

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

# Model 6 is Model 5 + remaining controls + singles ranking

mod6 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
            + Titles + Majors + Current + factor(Event.Type), family = binomial(link = logit), data = data)

# Models 7-9 are Model 6 with alternative specifications for Majors and French_Open to serve as robustness checks

mod7 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
            + Titles + Majors_Bi + Current + factor(Event.Type), family = binomial(link = logit), data = data)

mod8 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
            + Titles + Majors + Current_Bi + factor(Event.Type), family = binomial(link = logit), data = data)

mod9 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
            + Titles + Majors_Bi + Current_Bi + factor(Event.Type), family = binomial(link = logit), data = data)

# Running the (logit) models for singles players who were not 'substitutes' only

# Subsetting data for singles players who were not 'substitutes' only

subdata <- data[which(data$Lucky == 0),]

# Model 1 is just regressing gender on participation

submod1 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata)

# Model 2 is gender + age

submod2 <- glm(Competed ~ factor(Gender) + Age, family = binomial(link = logit), data = subdata)

# Model 3 is gender + age + career earnings

submod3 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1), family = binomial(link = logit), data = subdata)

# Model 4 is gender + age + YTD earnings

submod4 <- glm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata)

# Model 5 is gender + age + career earnings + YTD earnings

submod5 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata)

# Model 6 is Model 5 + remaining controls + singles ranking

submod6 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
               + Titles + Majors + Current + factor(Event.Type), family = binomial(link = logit), data = subdata)

# Models 7-9 are Model 6 with alternative specifications for Majors and French_Open to serve as robustness checks

submod7 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
               + Titles + Majors_Bi + Current + factor(Event.Type), family = binomial(link = logit), data = subdata)

submod8 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
               + Titles + Majors + Current_Bi + factor(Event.Type), family = binomial(link = logit), data = subdata)

submod9 <- glm(Competed ~ factor(Gender) + Age + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
               + Titles + Majors_Bi + Current_Bi + factor(Event.Type), family = binomial(link = logit), data = subdata)

# Generating  heteroskedasticity robust standard errors

cov <- vcovHC(mod1, type = 'HC0')
rse1 <- sqrt(diag(cov))

cov <- vcovHC(mod2, type = 'HC0')
rse2 <- sqrt(diag(cov))

cov <- vcovHC(mod3, type = 'HC0')
rse3 <- sqrt(diag(cov))

cov <- vcovHC(mod4, type = 'HC0')
rse4 <- sqrt(diag(cov))

cov <- vcovHC(mod5, type = 'HC0')
rse5 <- sqrt(diag(cov))

cov <- vcovHC(mod6, type = 'HC0')
rse6 <- sqrt(diag(cov))

cov <- vcovHC(mod7, type = 'HC0')
rse7 <- sqrt(diag(cov))

cov <- vcovHC(mod8, type = 'HC0')
rse8 <- sqrt(diag(cov))

cov <- vcovHC(mod9, type = 'HC0')
rse9 <- sqrt(diag(cov))

cov <- vcovHC(submod1, type = 'HC0')
subrse1 <- sqrt(diag(cov))

cov <- vcovHC(submod2, type = 'HC0')
subrse2 <- sqrt(diag(cov))

cov <- vcovHC(submod3, type = 'HC0')
subrse3 <- sqrt(diag(cov))

cov <- vcovHC(submod4, type = 'HC0')
subrse4 <- sqrt(diag(cov))

cov <- vcovHC(submod5, type = 'HC0')
subrse5 <- sqrt(diag(cov))

cov <- vcovHC(submod6, type = 'HC0')
subrse6 <- sqrt(diag(cov))

cov <- vcovHC(submod7, type = 'HC0')
subrse7 <- sqrt(diag(cov))

cov <- vcovHC(submod8, type = 'HC0')
subrse8 <- sqrt(diag(cov))

cov <- vcovHC(submod9, type = 'HC0')
subrse9 <- sqrt(diag(cov))

# Viewing regression results and saving to file

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, type = 'text', se = list(rse1, rse2, rse3, rse4, rse5, rse6, rse7, rse8, rse9))
stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7, submod8, submod9, type = 'text',
          se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7, subrse8, subrse9))

write.csv(stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, type = 'text', se = list(rse1, rse2, rse3, rse4, rse5, rse6, rse7, rse8, rse9)),
          paste(directory, 'results_all_full_season.txt', sep = ''), row.names = FALSE)
write.csv(stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7, submod8, submod9, type = 'text',
                    se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7, subrse8, subrse9)),
          paste(directory, 'results_lucky_full_season.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9,
                    se = list(rse1, rse2, rse3, rse4, rse5, rse6, rse7, rse8, rse9)),
          paste(directory, 'results_all_full_season_tex.txt', sep = ''), row.names = FALSE)
write.csv(stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7, submod8, submod9,
                    se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7, subrse8, subrse9)),
          paste(directory, 'results_lucky_full_season_tex.txt', sep = ''), row.names = FALSE)

