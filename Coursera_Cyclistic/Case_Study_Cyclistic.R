##Install necessary packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("janitor")
install.packages("readr")
install.packages("tidyr")
install.packages("lubridate")

##Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
library(tidyr)
library(lubridate)
library(readxl)

##Import data
Trips_2019_Q1 <- read_csv("~/Desktop/Coursera/Cyclistic_Trips_2019_Q1.csv")
Trips_2020_Q1 <- read_csv("~/Desktop/Coursera/Cyclistic_Trips_2020_Q1.csv")

#Quick look at data
colnames(Trips_2019_Q1)
colnames(Trips_2020_Q1)
##Note that there are some data variables that were not measured in 2020, like gender or date of birth.
head(Trips_2019_Q1)
head(Trips_2020_Q1)

#Ensure column names are all clean
clean_names(Trips_2019_Q1)
clean_names(Trips_2020_Q1)

##The next goal is to clean some of this data. 
##I adjusted column locations and names to allow the two spreadsheets to match.
##I also added columns to calculate trip_duration in minutes as well as days.

#Check out data tables
View(Trips_2019_Q1)
View(Trips_2020_Q1)

##Get some stats about trip duration based on customer type
Trips_2019_Q1 %>%
  group_by(usertype) %>%
  summarise(min_trip_duration = min(trip_duration_min),
            max_trip_duration = max(trip_duration_min),
            mean_trip_duration = mean(trip_duration_min))
##Subscribers appear to take shorter trips by a significant amount


Trips_2020_Q1 %>%
  group_by(usertype) %>%
  summarise(min_trip_duration = min(trip_duration_min),
            max_trip_duration = max(trip_duration_min),
            mean_trip_duration = mean(trip_duration_min))
##Here we also see a marked difference in time spent on the bikes by user type

##We also see that the minimum trip duration in 2020 is negative, so let's clean a bit more.
##All of the negative and most of the low duration trips appear to be happening at HQ QR - assuming that this is for testing purposes and not actual riders we are going to leave out any rides originating at HQ QR.
clean_Trips_2020_Q1 <- Trips_2020_Q1 %>%
  filter(start_station_name != "HQ QR")
##Now we can check our stats again...
clean_Trips_2020_Q1 %>%
  group_by(usertype) %>%
  summarise(min_trip_duration = min(trip_duration_min),
            max_trip_duration = max(trip_duration_min),
            mean_trip_duration = mean(trip_duration_min))


##Created a column to see which day of the week the trip was started on.
## =WEEKDAY(C2,1)   This outputs a number of 1-7 correlating with a day of the week.
## Translated those numbers into actual days using =IFS()


##After much cleaning, we are ready to merge our excel files
library(readxl)
Trips_2019_Q1_Clean <- read_excel("~/Desktop/Coursera/2019_Cleanest.xlsx")
Trips_2019_Q1_Clean$trip_id <- as.character(Trips_2019_Q1_Clean$trip_id)
##Needed to change the type of data in trip_id from dbl to chr to allow it to merge cleanly with 2020
Trips_2020_Q1_Clean <- read_excel("~/Desktop/Coursera/Cyclistic_Trips_2020_Q1_Clean.xlsx")
Merged_Trips <- bind_rows(Trips_2019_Q1_Clean, Trips_2020_Q1_Clean)

##This has some unnecessary and empty columns, so we're going to get rid of those
Merged_Trips_Clean<- Merged_Trips %>%
  drop_na(trip_id) %>%
  select(-gender,
         -birthyear,
         -bikeid)
  
##We finally have some workable data!
View(Merged_Trips_Clean)


##This works a bit, but not great
ggplot(data = Merged_Trips_Clean) +
  geom_jitter(mapping = aes(x=factor(start_day, days_of_the_week),
                            y=trip_duration_min,
                            color = usertype))


avg_rides <- Merged_Trips_Clean %>%
  select(trip_duration_min, start_time, start_day, usertype,) %>%
  mutate(year = year(start_time))

avg_rides %>%
  group_by(year, start_day, usertype) %>%
  summarise(average_ride = mean(trip_duration_min))


##This is soo much better
avg_rides_per_day <- Merged_Trips_Clean %>%
  mutate(day_of_week = wday(start_time, label = TRUE, abbr = FALSE),
         fiscal_year = year(start_time)) %>%
  group_by(fiscal_year,day_of_week,usertype) %>%
  summarise(average_duration = round(mean(trip_duration_min, na.rm = TRUE))) %>%
  ungroup()

view(avg_rides_per_day)

ggplot(avg_rides_per_day, 
       aes(x=day_of_week,
           y=average_duration,
           fill = factor(fiscal_year)))+
  geom_col(position = "dodge")+
  geom_text(aes(label = average_duration), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3,
            size = 8 / .pt,
            fontface = "bold")+
  facet_wrap(~usertype)+
  scale_fill_brewer(palette = "Pastel1")+
  labs(title = "Average Ride Duration by Day of the Week",
       x = "Day of the Week",
       y = "Average Ride Duration in Minutes",
       caption = "Cyclicity Q1 2019-2020",
       fill = "Year")+
  theme(axis.text.x = element_text(angle = 45))


##similar, but for count of rides
count_rides_per_day <- Merged_Trips_Clean %>%
  mutate(day_of_week = wday(start_time, label = TRUE, abbr = FALSE),
         fiscal_year = year(start_time)) %>%
  group_by(fiscal_year,day_of_week,usertype) %>%
  summarise(count_rides = n()) %>%
  ungroup()


ggplot(count_rides_per_day, 
       aes(x=day_of_week,
           y=count_rides,
           fill = factor(fiscal_year)))+
  geom_col(position = "dodge")+
  geom_text(aes(label = count_rides), 
            position = position_dodge(width = 0.9), 
            vjust = -0.4,
            size = 7 / .pt,
            fontface = "bold")+
  facet_wrap(~usertype)+
  scale_fill_brewer(palette = "Pastel2")+
  labs(title = "Total Rides by Day of the Week",
       x = "Day of the Week",
       y = "Count of Rides",
       caption = "Cyclicity Q1 2019-2020",
       fill = "Year")+
  theme(axis.text.x = element_text(angle = 45))


##a look at our riders in 2019
view(Trips_2019_Q1_Clean)

##get the current year to determine age
current_year <- as.numeric(format(Sys.Date(), "%Y"))

Riders_2019 <- Trips_2019_Q1_Clean %>%
  select(usertype, gender, birthyear) %>%
  mutate(age = current_year - birthyear) %>%
  drop_na() %>%
  group_by(age, gender, usertype) %>%
  ungroup()


ggplot(Riders_2019,
       aes(x=age,
           fill = gender))+
  geom_histogram(postion = "stack",
                 binwidth = 5,
                 color = "darkgray") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_x_continuous(breaks = seq(from = 20, to = 130, by = 10)) +
  facet_wrap(~usertype) +
  labs(title = "Age of Riders in 2019",
       x = "Age",
       y = "Count of Riders",
       caption = "Cyclicity 2019",
       fill = "Gender")

##Summary table of ages of riders 
Summary_Riders_2019 <- Riders_2019 %>%
  group_by(age, gender, usertype) %>%
  summarise(count_riders = n())
view(Summary_Riders_2019)

##Just the casuals
Summary_Riders_2019 %>%
  filter(usertype == "casual") %>%
ggplot(mapping = aes(x=age,
                     y=count_riders,
                     fill = gender))+
  geom_col(postion = "stack",
                 binwidth = 5,
                 color = "darkgray") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_x_continuous(breaks = seq(from = 20, to = 130, by = 10)) + 
  labs(title = "Age of Casual Riders in 2019",
       x = "Age",
       y = "Count of Riders",
       caption = "Cyclicity 2019",
       fill = "Gender")

##Just the members
Summary_Riders_2019 %>%
  filter(usertype == "member") %>%
  ggplot(mapping = aes(x=age,
                       y=count_riders,
                       fill = gender))+
  geom_col(postion = "stack",
           binwidth = 5,
           color = "darkgray") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_x_continuous(breaks = seq(from = 20, to = 130, by = 10)) + 
  labs(title = "Age of Members in 2019",
       x = "Age",
       y = "Count of Riders",
       caption = "Cyclicity 2019",
       fill = "Gender")




##Compare member/casual riders
total_riders_per_year <- Merged_Trips_Clean %>%
  group_by(year) %>%
  summarise(total_rides = n()) %>%
  ungroup ()
print(total_riders_per_year) ##works

total_riders<- Merged_Trips_Clean %>%
  group_by(year,
           usertype) %>%
  summarise(count_of_riders = n()) %>%
  left_join(total_riders_per_year, by = "year") %>%
  mutate(percentage = ((count_of_riders / total_rides) *100)) %>%
  ungroup ()
print(total_riders)




ggplot(total_riders,
       aes(x=year,
           y=percentage,
           fill=usertype)) +
  geom_col() + 
  geom_text(aes(label = paste(sprintf("%.1f",percentage), count_of_riders, sep = "% = ")), 
            vjust = -0.8,
            size = 12 / .pt,
            color = "white",
            fontface = "bold") +
  labs(title = "Percentage of Rider Membership",
       x = "Year",
       y = "Percent",
       fill = "Membership Status") +
  scale_fill_brewer(palette = "Accent") +
  scale_x_continuous(breaks = seq(from = 2019, to = 2020, by = 1))



