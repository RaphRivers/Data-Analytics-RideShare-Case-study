#Use the following code to duplicate the analysis
#In R Studio Begin by installing required packages and dependencies

install.packages("tidyverse") 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("janitor")
install.packages("readr")
install.packages("lubridate")

# Load package libraries and begin analysis
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(readr)

# Set working directory 
setwd("/kaggle/input/cylistic-bike-share-analysis/Raw Data")

# Load datasets to Download datasets see README
# Import raw datasets from working directory and assign name "trip0000_00"
trip2022_05 <- read_csv("202205-divvy-tripdata_1.csv")
trip2022_06 <- read_csv("202206-divvy-tripdata_2.csv")
trip2022_07 <- read_csv("202207-divvy-tripdata_3.csv")
trip2022_08 <- read_csv("202208-divvy-tripdata_4.csv")
trip2022_09 <- read_csv("202209-divvy-publictripdata_5.csv")
trip2022_10 <- read_csv("202210-divvy-tripdata_6.csv")
trip2022_11 <- read_csv("202211-divvy-tripdata_7.csv")
trip2022_12 <- read_csv("202212-divvy-tripdata_8.csv")
trip2023_01 <- read_csv("202301-divvy-tripdata_9.csv")
trip2023_02 <- read_csv("202302-divvy-tripdata_10.csv")
trip2023_03 <- read_csv("202303-divvy-tripdata_11.csv")
trip2023_04 <- read_csv("202304-divvy-tripdata_12.csv")

# Inspect and compare dataset columns
colnames(trip2022_05)
colnames(trip2022_06)
colnames(trip2022_07)
colnames(trip2022_08)
colnames(trip2022_09)
colnames(trip2022_10)
colnames(trip2022_11)
colnames(trip2022_12)
colnames(trip2023_01)
colnames(trip2023_02)
colnames(trip2023_03)
colnames(trip2023_04)

# Inspect the the file to look for incongruencies and data types for consistency
str(trip2022_05)
str(trip2022_06)
str(trip2022_07)
str(trip2022_08)
str(trip2022_09)
str(trip2022_10)
str(trip2022_11)
str(trip2022_12)
str(trip2023_01)
str(trip2023_02)
str(trip2023_03)
str(trip2023_04)

# Wrangle datasets into a single dataframe, ensure that all columns are consistent 
tripdata <- bind_rows(trip2022_05, trip2022_06, trip2022_07, trip2022_08,trip2022_09, trip2022_10, trip2022_11, trip2022_12, trip2023_01, trip2023_02, trip2023_03, trip2023_04)

# Use Glimpse function to view combined data frame
glimpse(tripdata)

# Inspect new dataframe 
colnames(tripdata) # List column name

#Get the number of rows and dimenssions
nrow(tripdata) # See the number of rows in the dataframe
dim(tripdata) # See the data dimension

# Inspect merged dataframe for analysis
str(tripdata) # See data frame columns and types

# Preview the first 6 rows of the new data frame
head(tripdata)

# Add new colunms for calculations
# Create new columns "ride_length", "day_of_week", "date", month", "Day", "Year" for analysis
tripdata$ride_length <- difftime(tripdata$ended_at, tripdata$started_at) # create trip_length column by subtracting trip start_at from end_at
tripdata$date <- as.Date(tripdata$started_at) # create date column
tripdata$month <- format(as.Date(tripdata$date), "%m") # create month column
tripdata$day <- format(as.Date(tripdata$date), "%d") # create day column
tripdata$year <- format(as.Date(tripdata$date), "%Y") # create year column
tripdata$day_of_week <- format(as.Date(tripdata$date), "%A") # create day of week column

# Inspect the columns structure
head(tripdata)

# Clean Dataframe, noite ride_length is in seconds, convert type to numeric to enable computation

# Convert ride_length type to numeric 
is.factor(tripdata$ride_length) # Check to see if type is numeric if not

# Set type as numeric
tripdata$ride_length <- as.numeric(as.character(tripdata$ride_length))

# Confirm type
is.numeric(tripdata$ride_length)

# Remove Delete unwanted columns and duplicates and null rows E.g. tripdata <- select(tripdata, - c("year"))
tripdata <- tripdata[!(tripdata$ride_length <= 0 ),] #remove negative values and 0 values in the “ride_length”column
cat("Number of rows :",nrow(tripdata)) # Count number of rows

# Remove any rows with missing data or with “NA”
tripdata_v1 <- tripdata <- na.omit(tripdata)

# Summarize data frame
summary(tripdata_v1)

# Summarize trip duration
summary(tripdata_v1$ride_length)


# Perform Descriptive Analysis, Avg ride length, midpoint in the assending array of ride length, logest/shortest ride and summary of ride length
mean(tripdata_v1$ride_length) #straight average (total ride length / rides)
median(tripdata_v1$ride_length)
max(tripdata_v1$ride_length)
min(tripdata_v1$ride_length)

# You can condense the four lines above to one line using summary() on the specific attribute
summary(tripdata_v1$ride_length)

# Compare Casusal amd Members Mean,Median,Minimum and Maximum ride length

# Mean
aggregate(tripdata_v1$ride_length ~ tripdata_v1$member_casual, FUN = mean)

# Median
aggregate(tripdata_v1$ride_length ~ tripdata_v1$member_casual, FUN = median)

# Min
aggregate(tripdata_v1$ride_length ~ tripdata_v1$member_casual, FUN = min)

# Max
aggregate(tripdata_v1$ride_length ~ tripdata_v1$member_casual, FUN = max)

# The average ride time for each day by members vs casual users
aggregate(tripdata_v1$ride_length ~ tripdata_v1$member_casual + tripdata_v1$day_of_week, FUN = mean)

# Organize analysis summary by day of week
tripdata_v1$day_of_week <- ordered(tripdata_v1$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Analyze Ridership data by type and weekday
aggregate(tripdata_v1ridelength tripdatav1
member_casual + tripdata_v1$day_of_week, FUN = mean)

#Export analysis for visualization
