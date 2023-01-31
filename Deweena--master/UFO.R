
#Loading necessary packages
library(dplyr)
library(lubridate)

# Get wd
setwd("/Users/deweenaparija/Desktop/MSC2011/Assignments")

# Read the csv to R and convert it to a dataframe.
df <- read.csv("ufo_subset.csv")
df <- as.data.frame(df)

# Check if column names have spaces - they don't, spaces are denoted as "."
# But changing it anyways so it looks better
colnames(df)
df <- df %>% 
  rename(duration_seconds = duration..seconds., duration_hrs_mins = duration..hours.min.)

# Convert date time and date posted to appropriate columns.
df$datetime <- as.Date(df$datetime)
df$date.posted <- as.Date(df$date.posted)
str(df)



UFO <- df %>%
# Filtering hoax sighting through comments; filtering by any comment with NUFORC note.
  filter(comments != "NUFORC") %>%
 
# Making a report_days column: date.posted - datetime
  mutate(report_delay = date.posted - datetime) %>%
  
# Filter out the rows where the sighting was reported before it happened (days_reported < 0 filtered out)
# Filtering rows without country input
  filter(report_delay >= 0, country != "") %>% 
 
# Grouping table by country 
  group_by(country) %>%

# Showing mean report_delays of each country 
   summarise(AVG_report_delay = mean(report_delay, na.rm = T)) 

# Checking data quality of duration seconds 
summary(df$duration_seconds) # There is a very large maxima seconds (82800000)
table(is.na(df$duration_seconds))  # No NA inputs
str(df$duration_seconds) # Data is numerical
hist(df$duration_seconds) # Because of the maximum, the rest of the frequencies are difficult to see.

# Plotting the log transformed duration_seconds displays a better graph
hist(log(df$duration_seconds), xlab = "Log transformed Duration in seconds", main = "Histogram of UFO sighting durations")


