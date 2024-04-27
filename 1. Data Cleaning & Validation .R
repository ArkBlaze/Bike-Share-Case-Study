#load packages
suppressMessages(library(tidyverse)) # data wrangling
library(visdat) # visualizing the aspects of a data frame. Here NAs
library(ggplot2) # plotting
suppressMessages(library(lubridate)) # working with dates and times
library(skimr) # summary statistics of variables and data frames
library(repr) # used for resizing plot area under code chunks
suppressMessages(library(scales)) # required for "labels = comma"


## Importing the Data
january_2022 <- read.csv("202201-divvy-tripdata.csv")
february_2022 <- read.csv("202202-divvy-tripdata.csv")
march_2022 <- read.csv("202203-divvy-tripdata.csv")
april_2022 <- read.csv("202204-divvy-tripdata.csv")
may_2022 <- read.csv("202205-divvy-tripdata.csv")
june_2022 <- read.csv("202206-divvy-tripdata.csv")
july_2022 <- read.csv("202207-divvy-tripdata.csv")
august_2022 <- read.csv("202208-divvy-tripdata.csv")
september_2022 <- read.csv("202209-divvy-tripdata.csv")
october_2022 <- read.csv("202210-divvy-tripdata.csv")
november_2022 <- read.csv("202211-divvy-tripdata.csv")
december_2022 <- read.csv("202212-divvy-tripdata.csv")


# Import all data and append into one data frame using bind_rows().
tripdata <- list.files(path = "C:/Users/Ark Blaze/Documents/R Projects/",
                       pattern =  "*.csv", full.names = TRUE) %>%
  lapply(read_csv, col_names = TRUE, show_col_types = FALSE) %>%
  bind_rows()  %>%
  rename("type_user" = member_casual) # rename the column that decribes the type of user



# Quick Skimming
head(tripdata_2022) # head of data frame
skim(tripdata_2022) # quick sum stats
#----------------------------------------------------------------------------------#


## Data Processing
# To make sure we work with high quality data, we will check for duplicates, missing values. 
duplicates_ride_id <- tripdata_2022[tripdata_2022$ride_id %in% tripdata_2022$ride_id[duplicated(tripdata_2022$ride_id)],]
duplicates_ride_id # print douplicate entries

# visualize NAs for first 30.000 rows
vis_miss(head(divy_tripdata_2021,30000))

# identify columns containing missing values
colnames(tripdata_2022)[colSums(is.na(tripdata_2022)) > 0]

# drop rows containing missing values
tripdata_2022 <- tripdata_2022 %>%
  drop_na(colnames(tripdata_2022)[colSums(is.na(tripdata_2022)) > 0])


#2.2 Add total trip time (trip_total)
tripdata_2022 <- tripdata_2022 %>%
  mutate(trip_total = round( difftime(ended_at, started_at, units = c('mins')),2))
head(tripdata_2022 %>% select(ride_id, type_user, trip_total)) # print head


#2.3 Classify trip duration
# Create new column "trip_class" containing a categorical label classifying trip length
tripdata <- tripdata %>%
  mutate(
    trip_class = case_when(
      trip_total < 1 ~ "logging error",
      trip_total >= 1 & trip_total < 10 ~ "short",
      trip_total >= 10 & trip_total < 60 ~ "medium",
      trip_total >= 60 & trip_total < 300 ~ "long",
      trip_total >= 300 & trip_total < 1440 ~ "very long",
      trip_total >= 1440  ~ "disappeared"))

#convert trip_calss to factor and order levels
tripdata <- tripdata %>%
  mutate(trip_class = factor(trip_class, levels = c("logging error", "short", "medium", "long", "very long", "disappeared")))
#----------------------------------------------------------------------------------#



## Plots


#1. Plot counts faceted by trip class

options(repr.plot.width = 10, repr.plot.height = 5) # set dimensions of plots
tripdata %>%
  ggplot(aes(x = hms::as_hms(trip_total), fill = trip_class, color = trip_class)) +
  geom_bar() +
  facet_wrap( ~ trip_class, scales = "free") +
  theme_classic() +
  scale_color_manual(values = c( '#FF1F5B', '#009ADC', '#AF58BA', '#FFC61E',  '#F28522', '#00CD6C')) +
  scale_fill_manual(values = c('#FF1F5B', '#009ADC', '#AF58BA', '#FFC61E', '#F28522', '#00CD6C'))


# Filter trip duration
# Exclude entries were there is either a negative time recorded, the trip duration is < 1 mins or > 1440 mins
times_are_off <-
  filter(tripdata, ended_at < started_at | trip_total < 1 | trip_total > 720)


# apply filter and remove times that are off
tripdata_clean <- tripdata %>%
  filter(!ride_id %in% times_are_off$ride_id) %>%
  filter(!start_station_name %in% 
           c('DIVVY CASSETTE REPAIR MOBILE STATION') |
           !end_station_name %in%
           c('DIVVY CASSETTE REPAIR MOBILE STATION'))


#2. Plot after filtering
tripdata_clean %>%
  ggplot(aes(x = hms::as_hms(trip_total), fill = trip_class, color = trip_class)) +
  geom_bar() +
  facet_wrap( ~ trip_class, scales = "free") +
  theme_classic() +
  scale_color_manual(values = c('#FF1F5B', '#009ADC', '#AF58BA', '#FFC61E', '#F28522', '#00CD6C')) +
  scale_fill_manual(values = c('#FF1F5B', '#009ADC', '#AF58BA', '#FFC61E', '#F28522', '#00CD6C'))

#----------------------------------------------------------------------------------#



## Add hour, week day, month columns
tripdata_final <- tripdata_clean %>%
  + mutate(hour = format(started_at, format = "%H"), day_week  = format(started_at, format = "%a"), day_month = format(started_at, format = "%d"), month = format(started_at, format = "%b"))

# Set factor levels for month & days. To display months & days in sequential order in plots (default is alphabetical).
tripdata_final <- tripdata_final %>%
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         day_week = factor(day_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

#----------------------------------------------------------------------------------#