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
  as_tibble()

# Analysis
## Getting average amount of time different rider types ride
df_stats <- df_clean %>% 
  group_by(member_casual) %>% 
  summarise(mean = as.duration(round(mean(ride_length))), 
            max = as.duration(round(max(ride_length))), 
            min = as.duration(round(min(ride_length)))
            ) 

# Wow.. 5 weeks?! There's definitely some outliers here.
  
  





  