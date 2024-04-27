##  ***Exploratory analysis***

#----------------------------------------------------------------------------------#
#load packages
suppressMessages(library(tidyverse)) # data wrangling 
library(visdat) # visualize aspects of a data frame. Here NAs
library(ggplot2) # plotting
suppressMessages(library(lubridate)) # working with dates and times
library(skimr) # summary statistics of variables and data frames
library(repr) # used for resizing plot area under code chunks
suppressMessages(library(scales)) # required for "labels = comma"
#----------------------------------------------------------------------------------#

#### **1. Number of trips by user type**
#### **1.1 Percent of trips across seasons**

# Calculate percent per month
trips_percent_month <- tripdata_final %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 1)) %>%
  mutate(percent_label = paste0(month, "\n", "(", round(100 * count / sum(count), 1), '%', ")")) %>%
  select(month, percent, percent_label)

# Calculate percent per season
trips_percent_season <- tripdata_final %>%
  mutate(season = case_when(month == "Jan" ~ "Winter",
                            month == "Feb" ~ "Winter",
                            month == "Mar" ~ "Spring",
                            month == "Apr" ~ "Spring",
                            month == "May" ~ "Spring",
                            month == "Jun" ~ "Summer",
                            month == "Jul" ~ "Summer",
                            month == "Aug" ~ "Summer",
                            month == "Sep" ~ "Fall",
                            month == "Oct" ~ "Fall",
                            month == "Nov" ~ "Fall",
                            month == "Dec" ~ "Winter")) %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>%
  group_by(season) %>%
  summarise(count = n()) %>%
  mutate(percent = round(100 * count / sum(count), 1)) %>%
  mutate(percent_label = paste0(season, "\n", "(", round(100 * count / sum(count), 1), '%', ")")) %>%
  select(season, percent, percent_label)
trips_percent_season[,3] # print 5 of trips per season


#### **1.2 Number of trips across the week and month**
# calculate summary statistics per month
sum_stats_month <- tripdata_final %>%
  group_by(type_user, month) %>%
  summarise(
    'trips' = n(),
    'time_mean' = mean(trip_total),
    'time_median' = median(trip_total),
    .groups = 'drop') %>%
  mutate(type_user = as.factor(type_user))

# calculate summary statistics per week
sum_stats_week <- tripdata_final %>%
  group_by(type_user, day_week) %>%
  summarise('trips' = n(),
            'time_mean' = mean(trip_total),
            'time_median' = median(trip_total), 
            .groups = 'drop') %>%
  mutate(type_user = as.factor(type_user))
#----------------------------------------------------------------------------------#



# Plot 1: Average trip length across the week
options(repr.plot.width = 30, repr.plot.height = 10) # set dimensions of plots

number_trips_week_plot <- sum_stats_week %>%
  ggplot(aes(x = day_week, y =  trips, group = type_user, color = type_user, fill = type_user)) +
  geom_line(aes(color = type_user), size = 1.5) + # add line to plot
  geom_point(size = 6) + # add dot plot
  theme_classic() + # apply classic theme
  scale_color_manual(values=c('#FF1F5B', '#009ADC')) +
  expand_limits(y = 0) + # force start of y axis at 0
  theme(plot.title = element_text(face = "bold", size = 30), #change title size and face
        plot.subtitle = element_text(face = "bold", size = 20), #change subtitle size and face
        axis.text = element_text(size = 20), # change axis label font size
        axis.title = element_text(size = 25, face = "bold"), # change axis title font size
        legend.title = element_text(size = 20), # change legend title font size
        legend.text = element_text(size = 16)) + # change legend text font size
  scale_y_continuous(labels = comma) + # format scale of y axis
  labs(title= "Number of trips throughout the week", subtitle = "Trips by casual riders peak during the weekend", x = "Day", y = "Number of trips", color = "User ", fill = "User ")


# Plot 2: Average trip length across the months
number_trips_months_plot <- sum_stats_month %>%
  ggplot(aes(x = month, y =  trips, group = type_user, color = type_user, fill = type_user)) +
  geom_line(aes(color = type_user), size = 1.5) + # add line to plot
  geom_vline(xintercept = c(0, 3, 6, 9, 12), linetype = 'dotted') + # add dotted line to delineate seasons
  geom_point(size = 6) + # add dot plot
  theme_classic() + # apply classic theme
  scale_color_manual(values=c('#FF1F5B', '#009ADC')) + # change color to custom values
  theme(plot.title = element_text(face = "bold", size = 30), # change title size and face
        plot.subtitle = element_text(face = "bold", size = 20), # change subtitle size and face
        axis.text = element_text(size = 20), # change axis label font size
        axis.title = element_text(size = 25, face = "bold"), # change axis title font size
        legend.title = element_text(size = 20), # change legend title font size
        legend.text = element_text(size = 16)) + # change legend text font size
  scale_y_continuous(labels = comma) + # format scale of y axis
  labs(title= "Number of trips across 2021", subtitle = "Trips by casual riders surpass members during summer", x = "Month", y = "Number of trips", color = "User ", fill = "User ") +
  annotate("text", x = c(1.5, 4.5, 7.5, 10.5), # x-coordinates of season labels
           y = c(1, 1, 1, 1), label = c("Winter", "Spring", "Summer", "Fall"), size = 10)


# Plot percent of trips per season
trips_percent_season$ymax <- cumsum(trips_percent_season$percent) # top of each rectangle
trips_percent_season$ymin <- c(0, head(trips_percent_season$ymax, n=-1)) # bottom of each rectangle
trips_percent_season$labelPosition <- (trips_percent_season$ymax + trips_percent_season$ymin) / 2 # label position

trips_percent_season_plot <- ggplot(trips_percent_season, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=season)) +
  geom_rect(color = "black") +
  geom_label( x=3.5, aes(y=labelPosition, label=percent_label), size=6,label.size = NA, alpha = 0) +
  scale_fill_manual(values=c( '#009ADC', '#F28522','#FFC61E','#00CD6C')) + # change fill to custom values
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title= "Percent of trips across seasons", subtitle = 'Less than 30% of trips occur during winter and spring') +
  theme(plot.title = element_text(face = "bold", size = 30, hjust = 0.5), plot.subtitle = element_text(face = "bold", size = 20, hjust = 0.5))
#----------------------------------------------------------------------------------#


#### **2 Average trip length by users **
options(repr.plot.width = 20, repr.plot.height = 10) # set dimensions of plots
# Plot 1: Average trip length across the Week
ggplot(data = sum_stats_week, aes(x = day_week, y =  `time_mean`, group = type_user, color = type_user, fill = type_user)) +
  geom_line(aes(color = type_user), linewidth = 1.5) + # add line to plot
  geom_point(size = 4) + # add dot plot
  theme_classic() + # apply classic theme
  scale_color_manual(values=c('#FF1F5B', '#009ADC')) + # change fill to custom values
  expand_limits(y = 0) + # force start of y axis at 0
  theme(plot.title = element_text(face = "bold", size = 20), # change title size and face
        plot.subtitle = element_text(face = "bold", size = 15), # change subtitle size and face
        axis.text = element_text(size = 10), # change axis label font size
        axis.title = element_text(size = 15, face = "bold"), # change axis title font size
        legend.title = element_text(size = 15),    # change legend title font size
        legend.text = element_text(size = 10)) + # change legend text font size
  scale_y_continuous() + # format scale of y axis
  labs(title = "Average trip length throughout the week", subtitle = 'Casual riders take longer trips', x = "Month", y = "Average trip length [min]", color = "User ", fill = "User ")

# Plot 2: Average trip length across the Month
ggplot(data = sum_stats_month, aes(x = month, y =  `time_mean`, group = type_user, color = type_user, fill = type_user)) +
  geom_line(aes(color = type_user), linewidth = 1.5) + # add line to plot
  geom_vline(xintercept = c(0, 3, 6, 9, 12), linetype = 'dotted') + # add dotted line to delineate seasons
  geom_point(size = 4) + # add dot plot
  theme_classic() + # apply classic theme
  scale_color_manual(values=c('#FF1F5B', '#009ADC')) + # change color to custom values
  theme(plot.title = element_text(face = "bold", size = 20), # change title size and face
        plot.subtitle = element_text(face = "bold", size = 15), # change subtitle size and face
        axis.text = element_text(size = 10), # change axis label font size
        axis.title = element_text(size = 15, face = "bold"), # change axis title font size
        legend.title = element_text(size = 15), # change legend title font size
        legend.text = element_text(size = 10)) + # change legend text font size
  scale_y_continuous() + # format scale of y axis
  labs(title = "Average trip length across 2022", subtitle = 'Casual riders take longer trips', x = "Month", y = "Average trip length [min]", color = "User ", fill = "User ") +
  annotate("text", x = c(1.5, 4.5, 7.5, 10.5), # x-coordinates of season labels
           y = c(1, 1, 1, 1), label = c("Winter", "Spring", "Summer", "Fall"), size = 6)
#----------------------------------------------------------------------------------#


#### **3. Top 25 trips**

# Make selection for trips that do not end at the start station
top_25_trips <-   tripdata_final %>%
  group_by(start_station_name, end_station_name) %>% # group by stations
  dplyr::summarize(count = n(), .groups = 'drop') %>% # count number of trips
  ungroup() %>% # remove grouping
  filter(start_station_name != end_station_name) %>% # filter for trips that do not end at the start station
  arrange(desc(count)) %>%
  top_n(25, wt = count)

# make table with updated header
top_25_trips_table <- top_25_trips %>%
  rename(`Start station` = start_station_name, `End station` = end_station_name, `Number of trips` = count)

# Make selection for trips that end at the start station
top_25_round_trips <- tripdata_final %>%
  group_by(start_station_name, end_station_name) %>% # group by stations
  dplyr::summarize(count = n(), .groups = 'drop') %>% # count number of trips
  ungroup() %>% # remove grouping
  filter(start_station_name == end_station_name) %>% # filter for trips that do not end at the start station
  arrange(desc(count)) %>%
  top_n(25, wt = count)

# make table with updated header
top_25_round_trips_table <- top_25_round_trips %>%
  rename(`Start station` = start_station_name, `End station` = end_station_name, `Number of trips` = count)

# Merge tables and arrange in descending order by "Number of trips"
top_25_trips_table %>% 
  mutate(`Trip type` = "one-way") %>%
  full_join(top_25_round_trips_table %>% 
              mutate(`Trip type` = "round trip"), by = c("Start station", "End station", "Number of trips", "Trip type")) %>%
  arrange(desc(`Number of trips`))

#----------------------------------------------------------------------------------#