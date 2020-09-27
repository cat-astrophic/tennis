# This script runs the statistical analyses for a paper on athletes, gender, and risk for the Italian Open only

# Loading libraries

library(sandwich)
library(stargazer)
library(margins)
library(ggplot2)

# Reading in the data

data <- read.csv(paste(directory, 'italian.csv', sep = ''), fileEncoding = 'UTF-8-BOM')

# Running the (logit) models for all players

# Model 1 is just regressing gender on participation

mod1 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = data)

# Model 2 is gender + age

mod2 <- glm(Competed ~ factor(Gender) + Age, family = binomial(link = logit), data = data)

# Model 3 is gender + age + career earnings

mod3 <- glm(Competed ~ factor(Gender) + Age + log(Winnings), family = binomial(link = logit), data = data)

# Model 4 is gender + age + YTD earnings

mod4 <- glm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), family = binomial(link = logit), data = data)

# Model 5 is gender + age + career earnings + YTD earnings

mod5 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1), family = binomial(link = logit), data = data)

# Model 6 is Model 5 + remaining controls + singles ranking

mod6 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1) + Doubles + COVID + Suspension + Qualifier
            + Injury + Ranking_S + Titles + Majors + Italian_Open + Prev_Tourn_Res + Prev_Tourn_Comp, family = binomial(link = logit), data = data)

# Models 7-9 are Model 6 with alternative specifications for Majors and Italian_Open to serve as robustness checks

mod7 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1) + Doubles + COVID + Suspension + Qualifier 
            + Injury + Ranking_S + Titles + Majors_Bi + Italian_Open + Prev_Tourn_Res + Prev_Tourn_Comp, family = binomial(link = logit), data = data)

mod8 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1) + Doubles + COVID + Suspension + Qualifier
            + Injury + Ranking_S + Titles + Majors + Italian_Open_Bi + Prev_Tourn_Res + Prev_Tourn_Comp, family = binomial(link = logit), data = data)

mod9 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1) + Doubles + COVID + Suspension + Qualifier
            + Injury + Ranking_S + Titles + Majors_Bi + Italian_Open_Bi + Prev_Tourn_Res + Prev_Tourn_Comp, family = binomial(link = logit), data = data)

# Running the (logit) models for singles players who were not 'substitutes' only

# Subsetting data for singles players who were not 'substitutes' only

subdata <- data[which(data$Lucky == 0),]

# Model 1 is just regressing gender on participation

submod1 <- glm(Competed ~ factor(Gender), family = binomial(link = logit), data = subdata)

# Model 2 is gender + age

submod2 <- glm(Competed ~ factor(Gender) + Age, family = binomial(link = logit), data = subdata)

# Model 3 is gender + age + career earnings

submod3 <- glm(Competed ~ factor(Gender) + Age + log(Winnings), family = binomial(link = logit), data = subdata)

# Model 4 is gender + age + YTD earnings

submod4 <- glm(Competed ~ factor(Gender) + Age + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata)

# Model 5 is gender + age + career earnings + YTD earnings

submod5 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1), family = binomial(link = logit), data = subdata)

# Model 6 is Model 5 + remaining controls + singles ranking

submod6 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1) + Doubles + COVID + Suspension + Qualifier
               + Injury + Ranking_S + Titles + Majors + Italian_Open + Prev_Tourn_Res + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata)

# Models 7-9 are Model 6 with alternative specifications for Majors and Italian_Open to serve as robustness checks

submod7 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1) + Doubles + COVID + Suspension + Qualifier 
               + Injury + Ranking_S + Titles + Majors_Bi + Italian_Open + Prev_Tourn_Res + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata)

submod8 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1) + Doubles + COVID + Suspension + Qualifier
               + Injury + Ranking_S + Titles + Majors + Italian_Open_Bi + Prev_Tourn_Res + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata)

submod9 <- glm(Competed ~ factor(Gender) + Age + log(Winnings) + log(Winnings_20 + 1) + Doubles + COVID + Suspension + Qualifier
               + Injury + Ranking_S + Titles + Majors_Bi + Italian_Open_Bi + Prev_Tourn_Res + Prev_Tourn_Comp, family = binomial(link = logit), data = subdata)

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

stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = 'text', se = list(rse1, rse2, rse3, rse4, rse5, rse6))
stargazer(submod1, submod2, submod3, submod4, submod5, submod6, type = 'text', se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6))

write.csv(stargazer(mod1, mod2, mod3, mod4, mod5, mod6, type = 'text',
                    se = list(rse1, rse2, rse3, rse4, rse5, rse6)),
          paste(directory, 'results_all_italian.txt', sep = ''), row.names = FALSE)
write.csv(stargazer(submod1, submod2, submod3, submod4, submod5, submod6, type = 'text',
                    se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6)),
          paste(directory, 'results_lucky_italian.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
                    se = list(rse1, rse2, rse3, rse4, rse5, rse6)),
          paste(directory, 'results_all_italian_tex.txt', sep = ''), row.names = FALSE)
write.csv(stargazer(submod1, submod2, submod3, submod4, submod5, submod6,
                    se = list(subrse1, subrse2, subrse3, subrse4, subrse5, subrse6)),
          paste(directory, 'results_lucky_italian_tex.txt', sep = ''), row.names = FALSE)

# Viewing the results from the robustness checks

stargazer(mod7, mod8, mod9, type = 'text', se = list(rse7, rse8, rse9))
stargazer(submod7, submod8, submod9, type = 'text', se = list(subrse7, subrse8, subrse9))

# Calculating the marginal effects

marg1 <- margins(mod1, type = 'response')
marg2 <- margins(mod2, type = 'response')
marg3 <- margins(mod3, type = 'response')
marg4 <- margins(mod4, type = 'response')
marg5 <- margins(mod5, type = 'response')
marg6 <- margins(mod6, type = 'response')

submarg1 <- margins(submod1, type = 'response')
submarg2 <- margins(submod2, type = 'response')
submarg3 <- margins(submod3, type = 'response')
submarg4 <- margins(submod4, type = 'response')
submarg5 <- margins(submod5, type = 'response')
submarg6 <- margins(submod6, type = 'response')

# View all marginal effects

marg1
marg2
marg3
marg4
marg5
marg6

submarg1
submarg2
submarg3
submarg4
submarg5
submarg6

