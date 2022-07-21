library(tidyverse)
library(janitor)
library(lubridate)
library(hms)

# Loading the data
path <- "./divvy-tripdata/"
filenames <- list.files(path = path)

## Into a list
all_df <- lapply(filenames, function(i) {
  i <- paste(path,i,sep="")
  read.csv(i, header=FALSE)
  })
filenames <- gsub("-","_",filenames)
names(all_df) <- gsub(".csv","",filenames)

## Joining all the data frames
joined_df <- all_df %>% 
  reduce(full_join)

names(joined_df) <- joined_df[1,]
df <- joined_df %>% 
  slice(-1)

# Cleaning
## Converting to correct data types, removing entries with NA
df_clean <- df %>% 
  mutate(started_at = ymd_hms(started_at)) %>% 
  mutate(ended_at = ymd_hms(ended_at)) %>% 
  mutate(start_lat = as.double(start_lat)) %>% 
  mutate(start_lat = as.double(start_lng)) %>%
  mutate(start_lat = as.double(end_lat)) %>%
  mutate(start_lat = as.double(end_lng)) %>%
  mutate(ride_length = as_hms(difftime(ended_at, started_at)), .after = ended_at) %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  na.omit() %>% 
  filter(started_at<=ended_at) %>% # remove cases where end time is somehow before the start time
  as_tibble()

# Analysis
## Getting average amount of time different rider types ride
df_stats <- df_clean %>% 
  # group_by(member_casual) %>% 
  summarise(mean = as.duration(round(mean(ride_length))), 
            max = as.duration(round(max(ride_length))), 
            min = as.duration(round(min(ride_length))),
            sd = as.duration(round(sd(ride_length))),
            median = as.duration(round(median(ride_length)))
            ) 

df_quantile <- df_clean %>% 
  # group_by(member_casual) %>% 
  summarise(q2 = as.duration(quantile(ride_length, 0.25)),
            q3 = as.duration(quantile(ride_length, 0.5)),
            q4 = as.duration(quantile(ride_length, 0.75)),
            iqr = as.duration(IQR(ride_length)),
            lower = q2-1.5*iqr, 
            min = as.duration(round(min(ride_length))),
            upper = q4+1.5*iqr,
            max = as.duration(round(max(ride_length)))
            )

# Wow.. 5 weeks?! There's definitely some outliers here.

# Filter Outliers
df_filter <- df_clean %>%
  subset(select = c(ride_id, rideable_type, started_at, ended_at, ride_length, member_casual)) %>% 
  filter(ride_length>df_quantile$upper) %>% 
  mutate(month = month(started_at, label = TRUE, abbr = TRUE))


# Number of Rides per month
df_filter %>% 
  group_by(member_casual, month) %>% 
  tally() %>% 
  ggplot() +
  geom_point(aes(x = month, y = n, color = member_casual)) +
  theme_minimal() +
  labs()

df_filter %>%
  group_by(member_casual, month) %>% 
  summarise(average_ride = mean(ride_length)) %>% 
  ggplot() +
  geom_point(aes(x = month, y = average_ride, color = member_casual))
  
  





  