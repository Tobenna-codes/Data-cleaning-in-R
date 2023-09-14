library(readxl)
library(ggplot2)

getwd()

setwd('../Downloads')

crime_data <- read_xls('./R course source files/12 - Excel Data Pipeline/data/FBI Crime Data.xls')

setwd('../Documents')

# Setting the correct column names for our dataframe

colnames(crime_data)<- crime_data[3,]

# Creating a data pipeline that filters our dataframe and leaves us with just
# the frame we want to work with

crime_data['converted__index'] <- sapply(crime_data[,1], as.numeric)

crime_data_filter <- crime_data[!is.na(crime_data$converted__index),]

# Cleaning our year column so it can be used as our row names

crime_data_filter$Year

crime_data_filter$Year =  substr(crime_data_filter$Year, 1, 4)

# Storing the years in a variable to be used for subsequent dataframes

Year <- crime_data_filter$Year

# Removing columns that are no longer needed

crime_data_filter <- subset(crime_data_filter, select = -c(Year, converted__index))

# Our data is not in the right datatype format so we need to do some conversions

str(crime_data_filter)

crime_data_filter <- as.data.frame(sapply(crime_data_filter, as.numeric))

summary(crime_data_filter)

# Assigning our 'Year' vector as the row names to our data frame

rownames(crime_data_filter) <- Year

# So we can do things like

crime_data_filter['2005',]

crime_data_filter['2010',c('Population1', 'Robbery')]

# We want to divide our data into two tables, one with the crime rate values and one with the nominal values
# Without hardcoding it of course

#check out the grepl function
?grepl

#defining a function that seperates our data based on specific pattern

sep_df <- function(pattern, x) {
  new_df <- x[,grepl(pattern = pattern, x = colnames(x), ignore.case = TRUE)]
  return(new_df)
}

rate_nominal_filter <- sep_df('rate', crime_data_filter)

crime_rate_data <- as.data.frame(crime_data_filter[,rate_nominal_filter])

nominal_crime_data <- as.data.frame(crime_data_filter[,!rate_nominal_filter])

# Rewriting our column names to be simpler and more meaning

colnames(nominal_crime_data)

colnames(nominal_crime_data) <- c(
  'Population',
  'Violent Crime',
  'Murder',
  'Robbery',
  'Aggravated Assault',
  'Property Crime',
  'Burglary',
  'Larceny Theft',
  'Motor Theft'
)

colnames(crime_rate_data)

colnames(crime_rate_data) <- c(
  'Violent Crime Rate',
  'Murder Rate',
  'Robbery Rate',
  'Aggravated Rate',
  'Property Crime Rate',
  'Burglary Rate',
  'Larceny Theft Rate',
  'Motor Theft Rate'
)

# Now using our cleaned data to perform analysis and visualizations

# We want to get the mean of our nominal crimes variables

mean_crime <- as.data.frame(sapply(nominal_crime_data, mean))

#We don't need the population

mean_crime <- tail(mean_crime, -1)

colnames(mean_crime) <- 'mean_value'

# Using ggplot2 to visualize our data

?geom_bar

(
ggplot(data = mean_crime/1000, aes(x = rownames(mean_crime), y = mean_value)) 
        + geom_bar(stat = 'identity', fill = 'darkorange')
        + xlab('Crimes') + ylab('Mean Value (K)') + labs(title = 'US Crime Reports', subtitle = '1997 - 2016') 
        + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
)


