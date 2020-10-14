# Main script for a paper on athletes, gender, and risk

# Specifying the directory for the data  -- ensure that the directory is correct

directory <- 'C:/Users/User/Documents/Data/tennis/'

# Running the individual scripts for each set of analyses -- ensure that the directory is correct

diRectoRy <- 'C:/Users/User/Documents/'

# Running the R scripts

source(paste(diRectoRy, 'us_open.R', sep = ''))
source(paste(diRectoRy, 'italian_open.R', sep = ''))
source(paste(diRectoRy, 'french_open.R', sep = ''))
source(paste(diRectoRy, 'slams.R', sep = ''))
source(paste(diRectoRy, 'full_season.R', sep = ''))
source(paste(diRectoRy, 'full_season_grouped.R', sep = ''))
source(paste(diRectoRy, 'full_season_GR.R', sep = ''))
