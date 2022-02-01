##
##    Project:    MCR Nutrient Synoptic Survey
##
##    Name:       stream_distance.R
##
##    Objective:  Find distance from MCR sample sites to the nearest stream mouth
##
##    Approach:   Utilize latitude/longitude data from sample locations and 
##                stream mouth locations and find shortest distance using 
##                Haversine distances. 
##                
##    Authors:    Maya E. Zeff & Hendrikje Jorissen
##
##    Date:       January 30, 2022


# 1. Set up ---------------------------------------------------------------


## load Tidyverse
library(tidyverse)
## load here
library(here)
## For determining Haversine distances
library("geosphere")

## read in stream mouth coordinates
stream_sites <- read_csv(here("Data","stream_sites.csv")) %>%
  # rename lat & long to be stream specific
  mutate(lon_stream = Lon, lat_stream = Lat, stream_name = name) %>%
  # select lat, long, & location name for streamlining
  select(stream_name, lon_stream, lat_stream) %>%
  # filter only stream mouth names from dataset
   filter(stream_name %in% c("Maharepa Stream Mouth", 
                             "Vaiare Stream Mouth", 
                             "Afareaitu Stream Mouth", 
                             "Piahena River Mouth", 
                             "Teavaro Stream Mouth", 
                             "Haumi Stream Mouth", 
                             "Maatea Stream Mouth", 
                             "Atiha Stream Mouth", 
                             "Vaiane Stream Mouth",
                             "Haapiti Stream Mouth", 
                             "Oponohu Stream Mouth", 
                             "Pao Pao Stream Mouth", 
                             "Papetoai Stream Mouth", 
                             "Nuurua Stream Mouth", 
                             "Moana Stream Mouth", 
                             "Small Stream 1 Mouth", 
                             "Golf Course Stream Mouth"))

## read in MCR nutrient sample coordinates
nutrient_data<-read_csv("https://raw.githubusercontent.com/njsilbiger/NutrientRegimes/main/Data/NutrientAll.csv") %>% 
  # rename lat & long to be stream specific
  mutate(lon_sample = Lon, lat_sample = Lat) %>%
  # select lat, long, & site number for streamlining
  select(Site_Number, lon_sample, lat_sample)

## expand nutrient sample data frame
  # lat & long for each sample number repeat for the length of stream-mouth lat & long data frame (nutrient_data)
  long_nutrient <- nutrient_data[rep(seq_len(nrow(nutrient_data)), each = nrow(stream_sites)), ] 

## expand stream mouth data frame
  # lat & long for each stream repeat in a sequence n times, where n = the length of stream mouth lat & long data frame (stream_sites)
  long_stream <- do.call("rbind", replicate(nrow(nutrient_data), stream_sites, simplify = FALSE))

# 2. Find distances  -------------------------------------------------------

## bind expanded nutrient lat long data frame with expanded stream  mouth lat long data frame 
long_total <- cbind(long_nutrient, long_stream) %>% 
    # find Haversine distance
    mutate(stream_dist_m = distHaversine(cbind(lon_stream, lat_stream), cbind(lon_sample, lat_sample))) %>%
    # group by sample Site Number
    group_by(Site_Number) %>%
    # choose only minimum distances
    slice(which.min(stream_dist_m)) %>%
    # select site number, stream mouth name, and distance
    select(Site_Number, stream_name, stream_dist_m)

## add nearest stream mouth name and distance to original MCR nutrient dataset 
nutrient_stream_data <- merge(nutrient_data, long_total, by = "Site_Number")

#write_csv(nutrient_stream_data, here("Data","nutrient_stream_dist.csv"))


