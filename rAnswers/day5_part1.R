# Day 5 Long story short --- this is my favorite solution for part 1, but I did
# not yet get part 2 and have gotten pretty frustrated! I understand the concept
# and plan to return, but want to move on. So I've included part 1 here, and my non-working part 2 in progress 

library(tidyverse)

# input <- readLines("input/day5_example_input.txt")
input <- readLines("input/day5_input.txt")

input <- c(input, "") # add 1 line for final processing
source("rAnswers/functions/seed_to_location.R")
source("rAnswers/functions/generate_conversion_df.R")

# Generate conversion charts as a single df

all_maps_df <- generate_conversion_df(input)

# Chart names for parsing

map_names <-
  c(
    "seed-to-soil",
    "soil-to-fertilizer",
    "fertilizer-to-water",
    "water-to-light",
    "light-to-temperature",
    "temperature-to-humidity"
  )

# Part 1

# Read in each seed individually and work through the charts
# At the end of each conversion --- ask, is it the lowest so far?

# Get seeds as vector and sort
seeds <- str_extract_all(input[1], "\\d+") %>%
  unlist() %>%
  as.numeric() %>%
  sort()

lowest_value <- c()

for (seed in seeds){
  value <- seed_to_location(seed, map_names, all_maps_df)
  if (length(lowest_value) == 0){
    lowest_value <- value
  } else if (value < lowest_value){
    lowest_value <- value
  }
}

print(lowest_value)