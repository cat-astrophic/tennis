# Main script for a paper on athletes, gender, and risk

# Enter your username

username <- ''

# Specifying the directory forthe data  -- ensure that the directory is correct

directory <- paste('C:/Users/', username, '/Documents/Data/tennis/', sep = '')

# Running the individual scripts for each set of analyses -- ensure that the directory is correct

diRectoRy <- paste('C:/Users/', username, '/Documents/', sep = '')

# Running the R scripts

source(paste(diRectoRy, 'tennis_main.R', sep = ''))
source(paste(diRectoRy, 'tennis_sensitive.R', sep = ''))
source(paste(diRectoRy, 'tennis_plots.R', sep = ''))

