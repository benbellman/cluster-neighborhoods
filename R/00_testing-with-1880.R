library(here)
library(sf)
library(rio)
library(dplyr)
library(furrr)
library(reshape2)


source(here("R", "functions", "calc_neighborhood.R"))

#### Initial test
## cal_neighborhood currently using a binary distance, default max is 100 meters / units
## Jan 23 2019

# load in off street geocoding
points <- st_read(here("data", "1880", "Off Street","Philadelphia_OffStreet_BenResult.shp"), stringsAsFactors = F) %>% 
  # transform to UTM, meter units
  st_transform(26918) %>% 
  # keep only successful geocodes
  filter(Status == "M")

# load microdata
micro <- import(here("data", "1880", "Match Address","Philadelphia_BenResult.csv")) %>% 
  # add some basic columns for calculations
  mutate(race_grp = case_when(race == "200" ~ "Black",
                              race == "210" ~ "Black",
                              race == "100" ~ "White",
                              TRUE ~ "Other"),
         black = if_else(race_grp == "Black", 1, 0),
         white_frnbrn = if_else(race_grp == "White" & bpldet >= 15000, 1, 0))
# replace sei values of zero with NA
micro$seius[micro$seius == 0] <- NA

# test full program with a sample of points
#test_sample <- sample_n(points, 15)

# set future option for multi-threading
plan(tweak(multiprocess, workers = 12))
options(future.globals.maxSize = 2 * 1024^3)

# apply function with future version of map from furrr package
test_results <- points %>% 
  split(1:nrow(.)) %>% 
  future_map_dfr(calc_neighborhood, points = points, micro = micro)

# output results
export(test_results, here("data", "results", "test-neighborhoods-1880.csv"))

