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

# Adding an Age^2 variable to the data set

data$Age2 <- data$Age*data$Age

# Dropping individuals who did not choose whether or not to participate, i.e., those who had COVID, an injury, or were suspended

data <- data[which(data$COVID == 0),]
data <- data[which(data$Injury == 0),]
data <- data[which(data$Suspension == 0),]

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

# Model 6 is Model 5 + remaining controls + singles ranking

mod6 <- glm(Competed ~ factor(Gender) + Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
            + Titles + Majors + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = data)

# Models 7-9 are Model 6 with alternative specifications for Majors and French_Open to serve as robustness checks

mod7 <- glm(Competed ~ factor(Gender) + Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
            + Titles + Majors_Bi + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = data)

mod8 <- glm(Competed ~ factor(Gender) + Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
            + Titles + Majors + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = data)

mod9 <- glm(Competed ~ factor(Gender) + Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
            + Titles + Majors_Bi + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = data)

# Running the (logit) models for singles players who were not 'substitutes' only

# Subsetting data for singles players who were not 'substitutes' only

subdata <- data[which(data$Lucky == 0),]
subdatam <- datam[which(data$Lucky == 0),]
subdataf <- dataf[which(data$Lucky == 0),]

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

submod6 <- glm(Competed ~ factor(Gender) + Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
               + Titles + Majors + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata)

# Models 7-9 are Model 6 with alternative specifications for Majors and French_Open to serve as robustness checks

submod7 <- glm(Competed ~ factor(Gender) + Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
               + Titles + Majors_Bi + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata)

submod8 <- glm(Competed ~ factor(Gender) + Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
               + Titles + Majors + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata)

submod9 <- glm(Competed ~ factor(Gender) + Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
               + Titles + Majors_Bi + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata)

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

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, type = 'text', se = list(rse1, rse2, rse3, rse4, rse5, rse6, rse7, rse8, rse9))
stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7, submod8, submod9, type = 'text',
          se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7, subrse8, subrse9))

# Repeat all regressions for women and men only

mod2m <- glm(Competed ~ Age, family = binomial(link = logit), data = datam)

mod3m <- glm(Competed ~ Age + log(Winnings + 1), family = binomial(link = logit), data = datam)

mod4m <- glm(Competed ~ Age + log(Winnings_20 + 1), family = binomial(link = logit), data = datam)

mod5m <- glm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = datam)

mod6m <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
             + Titles + Majors + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = datam)

mod7m <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
             + Titles + Majors_Bi + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = datam)

mod8m <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
             + Titles + Majors + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = datam)

mod9m <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
             + Titles + Majors_Bi + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = datam)

mod2f <- glm(Competed ~ Age, family = binomial(link = logit), data = dataf)

mod3f <- glm(Competed ~ Age + log(Winnings + 1), family = binomial(link = logit), data = dataf)

mod4f <- glm(Competed ~ Age + log(Winnings_20 + 1), family = binomial(link = logit), data = dataf)

mod5f <- glm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = dataf)

mod6f <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
             + Titles + Majors + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = dataf)

mod7f <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
             + Titles + Majors_Bi + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = dataf)

mod8f <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
             + Titles + Majors + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = dataf)

mod9f <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
             + Titles + Majors_Bi + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = dataf)

submod2m <- glm(Competed ~ Age, family = binomial(link = logit), data = subdatam)

submod3m <- glm(Competed ~ Age + log(Winnings + 1), family = binomial(link = logit), data = subdatam)

submod4m <- glm(Competed ~ Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdatam)

submod5m <- glm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdatam)

submod6m <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
                + Titles + Majors + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdatam)

submod7m <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
                + Titles + Majors_Bi + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdatam)

submod8m <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
                + Titles + Majors + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdatam)

submod9m <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
                + Titles + Majors_Bi + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdatam)

submod2f <- glm(Competed ~ Age, family = binomial(link = logit), data = subdataf)

submod3f <- glm(Competed ~ Age + log(Winnings + 1), family = binomial(link = logit), data = subdataf)

submod4f <- glm(Competed ~ Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdataf)

submod5f <- glm(Competed ~ Age + log(Winnings + 1) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdataf)

submod6f <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
                + Titles + Majors + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdataf)

submod7f <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
                + Titles + Majors_Bi + Current + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdataf)

submod8f <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
                + Titles + Majors + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdataf)

submod9f <- glm(Competed ~ Age + Age2 + log(Winnings + 1) + log(Winnings_20 + 1) + Doubles + Qualifier + Ranking_S
                + Titles + Majors_Bi + Current_Bi + factor(Event.Type) + Prev_Tourn_Comp, family = binomial(link = logit), data = subdataf)

# Generating  heteroskedasticity robust standard errors

cov <- vcovHC(mod2m, type = 'HC0')
rse2m <- sqrt(diag(cov))

cov <- vcovHC(mod3m, type = 'HC0')
rse3m <- sqrt(diag(cov))

cov <- vcovHC(mod4m, type = 'HC0')
rse4m <- sqrt(diag(cov))

cov <- vcovHC(mod5m, type = 'HC0')
rse5m <- sqrt(diag(cov))

cov <- vcovHC(mod6m, type = 'HC0')
rse6m <- sqrt(diag(cov))

cov <- vcovHC(mod7m, type = 'HC0')
rse7m <- sqrt(diag(cov))

cov <- vcovHC(mod8m, type = 'HC0')
rse8m <- sqrt(diag(cov))

cov <- vcovHC(mod9m, type = 'HC0')
rse9m <- sqrt(diag(cov))

cov <- vcovHC(mod2f, type = 'HC0')
rse2f <- sqrt(diag(cov))

cov <- vcovHC(mod3f, type = 'HC0')
rse3f <- sqrt(diag(cov))

cov <- vcovHC(mod4f, type = 'HC0')
rse4f <- sqrt(diag(cov))

cov <- vcovHC(mod5f, type = 'HC0')
rse5f <- sqrt(diag(cov))

cov <- vcovHC(mod6f, type = 'HC0')
rse6f <- sqrt(diag(cov))

cov <- vcovHC(mod7f, type = 'HC0')
rse7f <- sqrt(diag(cov))

cov <- vcovHC(mod8f, type = 'HC0')
rse8f <- sqrt(diag(cov))

cov <- vcovHC(mod9f, type = 'HC0')
rse9f <- sqrt(diag(cov))

cov <- vcovHC(submod2m, type = 'HC0')
subrse2m <- sqrt(diag(cov))

cov <- vcovHC(submod3m, type = 'HC0')
subrse3m <- sqrt(diag(cov))

cov <- vcovHC(submod4m, type = 'HC0')
subrse4m <- sqrt(diag(cov))

cov <- vcovHC(submod5m, type = 'HC0')
subrse5m <- sqrt(diag(cov))

cov <- vcovHC(submod6m, type = 'HC0')
subrse6m <- sqrt(diag(cov))

cov <- vcovHC(submod7m, type = 'HC0')
subrse7m <- sqrt(diag(cov))

cov <- vcovHC(submod8m, type = 'HC0')
subrse8m <- sqrt(diag(cov))

cov <- vcovHC(submod9m, type = 'HC0')
subrse9m <- sqrt(diag(cov))

cov <- vcovHC(submod2f, type = 'HC0')
subrse2f <- sqrt(diag(cov))

cov <- vcovHC(submod3f, type = 'HC0')
subrse3f <- sqrt(diag(cov))

cov <- vcovHC(submod4f, type = 'HC0')
subrse4f <- sqrt(diag(cov))

cov <- vcovHC(submod5f, type = 'HC0')
subrse5f <- sqrt(diag(cov))

cov <- vcovHC(submod6f, type = 'HC0')
subrse6f <- sqrt(diag(cov))

cov <- vcovHC(submod7f, type = 'HC0')
subrse7f <- sqrt(diag(cov))

cov <- vcovHC(submod8f, type = 'HC0')
subrse8f <- sqrt(diag(cov))

cov <- vcovHC(submod9f, type = 'HC0')
subrse9f <- sqrt(diag(cov))

# Writing results ot file

write.csv(stargazer(mod2m, mod3m, mod4m, mod5m, mod6m, mod7m, mod8m, mod9m, type = 'text',
                    se = list(rse2m, rse3m, rse4m, rse5m, rse6m, rse7m, rse8m, rse9m)),
          paste(directory, 'results_all_full_season_M.txt', sep = ''), row.names = FALSE)
write.csv(stargazer(submod2m, submod3m, submod4m, submod5m, submod6m, submod7m, submod8m, submod9m, type = 'text',
                    se = list(subrse2m, subrse3m, subrse4m, subrse5m, subrse6m, subrse7m, subrse8m, subrse9m)),
          paste(directory, 'results_lucky_full_season_M.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(mod2m, mod3m, mod4m, mod5m, mod6m, mod7m, mod8m, mod9m,
                    se = list(rse2m, rse3m, rse4m, rse5m, rse6m, rse7m, rse8m, rse9m)),
          paste(directory, 'results_all_full_season_M_tex.txt', sep = ''), row.names = FALSE)
write.csv(stargazer(submod2m, submod3m, submod4m, submod5m, submod6m, submod7m, submod8m, submod9m,
                    se = list(subrse2m, subrse3m, subrse4m, subrse5m, subrse6m, subrse7m, subrse8m, subrse9m)),
          paste(directory, 'results_lucky_full_season_M_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(mod2f, mod3f, mod4f, mod5f, mod6f, mod7f, mod8f, mod9f, type = 'text',
                    se = list(rse2f, rse3f, rse4f, rse5f, rse6f, rse7f, rse8f, rse9f)),
          paste(directory, 'results_all_full_season_F.txt', sep = ''), row.names = FALSE)
write.csv(stargazer(submod2f, submod3f, submod4f, submod5f, submod6f, submod7f, submod8f, submod9f, type = 'text',
                    se = list(subrse2f, subrse3f, subrse4f, subrse5f, subrse6f, subrse7f, subrse8f, subrse9f)),
          paste(directory, 'results_lucky_full_season_F.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(mod2f, mod3f, mod4f, mod5f, mod6f, mod7f, mod8f, mod9f,
                    se = list(rse2f, rse3f, rse4f, rse5f, rse6f, rse7f, rse8f, rse9f)),
          paste(directory, 'results_all_full_season_F_tex.txt', sep = ''), row.names = FALSE)
write.csv(stargazer(submod2f, submod3f, submod4f, submod5f, submod6f, submod7f, submod8f, submod9f,
                    se = list(subrse2f, subrse3f, subrse4f, subrse5f, subrse6f, subrse7f, subrse8f, subrse9f)),
          paste(directory, 'results_lucky_full_season_F_tex.txt', sep = ''), row.names = FALSE)

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, type = 'text', se = list(rse1, rse2, rse3, rse4, rse5, rse6, rse7, rse8, rse9))
stargazer(submod1, submod2, submod3, submod4, submod5, submod6, submod7, submod8, submod9, type = 'text',
          se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6, subrse7, subrse8, subrse9))

stargazer(mod2m, mod3m, mod4m, mod5m, mod6m, mod7m, mod8m, mod9m, type = 'text',
          se = list(rse2m, rse3m, rse4m, rse5m, rse6m, rse7m, rse8m, rse9m))
stargazer(submod2m, submod3m, submod4m, submod5m, submod6m, submod7m, submod8m, submod9m, type = 'text',
          se = list(subrse2m, subrse3m, subrse4m, subrse5m, subrse6m, subrse7m, subrse8m, subrse9m))

stargazer(mod2f, mod3f, mod4f, mod5f, mod6f, mod7f, mod8f, mod9f, type = 'text',
          se = list(rse2f, rse3f, rse4f, rse5f, rse6f, rse7f, rse8f, rse9f))
stargazer(submod2f, submod3f, submod4f, submod5f, submod6f, submod7f, submod8f, submod9f, type = 'text',
          se = list(subrse2f, subrse3f, subrse4f, subrse5f, subrse6f, subrse7f, subrse8f, subrse9f))

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
mar7 <- margins(submod7)
mar8 <- margins(submod8)
mar9 <- margins(submod9)

mar2f <- margins(submod2f)
mar3f <- margins(submod3f)
mar4f <- margins(submod4f)
mar5f <- margins(submod5f)
mar6f <- margins(submod6f)
mar7f <- margins(submod7f)
mar8f <- margins(submod8f)
mar9f <- margins(submod9f)

mar2m <- margins(submod2m)
mar3m <- margins(submod3m)
mar4m <- margins(submod4m)
mar5m <- margins(submod5m)
mar6m <- margins(submod6m)
mar7m <- margins(submod7m)
mar8m <- margins(submod8m)
mar9m <- margins(submod9m)

# t-statistics

t1 <- submod1$coefficients / subrse1
t2 <- submod2$coefficients / subrse2
t3 <- submod3$coefficients / subrse3
t4 <- submod4$coefficients / subrse4
t5 <- submod5$coefficients / subrse5
t6 <- submod6$coefficients / subrse6
t7 <- submod7$coefficients / subrse7
t8 <- submod8$coefficients / subrse8
t9 <- submod9$coefficients / subrse9

t2f <- submod2f$coefficients / subrse2f
t3f <- submod3f$coefficients / subrse3f
t4f <- submod4f$coefficients / subrse4f
t5f <- submod5f$coefficients / subrse5f
t6f <- submod6f$coefficients / subrse6f
t7f <- submod7f$coefficients / subrse7f
t8f <- submod8f$coefficients / subrse8f
t9f <- submod9f$coefficients / subrse9f

t2m <- submod2m$coefficients / subrse2m
t3m <- submod3m$coefficients / subrse3m
t4m <- submod4m$coefficients / subrse4m
t5m <- submod5m$coefficients / subrse5m
t6m <- submod6m$coefficients / subrse6m
t7m <- submod7m$coefficients / subrse7m
t8m <- submod8m$coefficients / subrse8m
t9m <- submod9m$coefficients / subrse9m

