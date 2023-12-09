# I'm interested in trying this without actually creating any permanent objects

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

conversion_vector <- c()
current_seeds <- sort(seeds)

# Parse input and populate df
for (line in input[3:35]) {
  # create all_maps_df from input
  gc()
  if (str_starts(line, "\\D")) {
    map_type <-
      str_extract(line, "\\w+-\\w+-\\w+") # if line is text, set map_type
  } else if (line == "") {
    if (length(current_seeds > 0)) {
      conversion_vector <-
        append(conversion_vector, current_seeds) # populate unfound items with duplicates
      current_seeds <-
        conversion_vector # new "current_seeds" for the next section of data
      conversion_vector <- c()
    } else {
      current_seeds <- conversion_vector
      conversion_vector <- c()
    }
  } else {
    data_to_parse <-
      str_extract_all(line, "\\d+") %>% unlist() %>% as.numeric()  # parse destination, source, range
    destination <- data_to_parse[1]
    source = data_to_parse[2]
    range = data_to_parse[3]
    
    temp_tibble <- tibble(
      source = seq(source, source + (range - 1)),
      destination = seq(destination, destination + (range - 1))
    ) %>%
      arrange(source)
    
    print(paste(c("current line  ", line)))
    
    for (seed in current_seeds) {
      print(paste(c("looking for", seed)))
      if (seed >= source & seed <= source + range) {
        print("Found one!")
        conversion_vector <-
          append(
            conversion_vector,
            filter(temp_tibble, source == seed) %>%
              select(destination) %>%
              as.numeric()
          )
        current_seeds <- current_seeds[! current_seeds %in% seed]
        
      } else if (seed > source + range){
        print(paste(c("limit is ", seed)))
        break
      }
        else {
        next
        }
      
      
      
      # if (seed %in% temp_tibble$source){
      #         loc <- which(temp_tibble$source == seed)
      # conversion_vector <- append(conversion_vector, temp_tibble$destination[loc])
      # current_seeds <- current_seeds[! current_seeds %in% seed]
    }
    rm(temp_tibble)
    }
  
}

final_locations <- c(conversion_vector, current_seeds)
print(min(final_locations))


#

# # Seed to location lookup
# locations <- c()
# for (seed in seeds){
#   locations <- append(locations, seed_to_location(seed))
# }
# 
# print(min(locations)) # final answer