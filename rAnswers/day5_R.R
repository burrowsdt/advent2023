library(tidyverse)

# input <- readLines("~/Documents/repos/advent2023/input/day5_example_input.txt")
input <- readLines("~/Documents/repos/advent2023/input/day5_input.txt")
source("~/Documents/repos/advent2023/rAnswers/functions/seed_to_location.R")

# Get seeds as vector
seeds <- str_extract_all(input[1], "\\d+") %>%
  unlist() %>% 
  as.numeric()

# Initialize df
map_names <- c("seed-to-soil", "soil-to-fertilizer", "fertilizer-to-water","water-to-light","light-to-temperature","temperature-to-humidity")
all_maps_df <- tibble(
  "map_type" = character(),
  "source" = numeric(),
  "destination" = numeric(),
)

# we don't know exactly how big our final df will be, so can't pre-allocate. We do some pre-allocation, however, by counting the number of lines we need to process that are not text or blank
number_of_drows <- length(which(str_starts(input, "\\d") == TRUE))

df_list <- vector(mode = "list", length = number_of_drows)
i <- 1
# Parse input and populate df
for (line in input[3:length(input)]){ # create all_maps_df from input
  if (str_starts(line, "\\D")){
    map_type <- str_extract(line, "\\w+-\\w+-\\w+") # if line is text, set map_type
  } else if (line == ""){
    next
  } else {
    data_to_parse <- str_extract_all(line, "\\d+") %>% unlist() %>% as.numeric()  # parse destination, source, range
    destination <- data_to_parse[1]
    source = data_to_parse[2]
    range = data_to_parse[3]

    
    df_list[[i]] <- tibble(
      map_type = map_type, 
      destination = seq(destination, destination + (range - 1)),
      source = seq(source, source + (range - 1))
    )
    
    i <- i + 1
    
    # 
    # all_maps_df <- add_row(
    #   all_maps_df,
    #   map_type = map_type,
    #   destination = destination,
    #   source = source
    # )

    
        # 
    # if (range > 1){
    #   for (i in seq(2:range-1)){
    #     all_maps_df <- add_row(
    #       all_maps_df,
    #       map_type = map_type,
    #       destination = destination + i,
    #       source = source + i,
    #     )   
    #   }  
    # }
  }
}

all_maps_df <- purrr::list_rbind(df_list)

# Seed to location lookup
locations <- c()
for (seed in seeds){
  locations <- append(locations, seed_to_location(seed))
}

print(min(locations)) # final answer