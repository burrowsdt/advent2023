# Day 5 - If You Give a Seed a Fertilizer - Revised, but still a handful

# Here, I'm just relying on math to "expand" the ranges. The lesson is huge:
# trying to expand and iterate over either dfs or tables for this solution is
# virtually impossible because of the size of them. But the below code runs for part 1 like butter.

# It's not that I *didn't* know this at all... but I was surprised at how drastic the difference.

# Part 2 adds a twist --- bc the size of the seed set is so big, it makes my old approach somewhat obsolete.


library(tidyverse)

input <- readLines("~/Repos/advent2023/input/day5_example_input.txt")
# input <- readLines("~/Repos/advent2023/input/day5_input.txt")

input <- c(input, "") # add 1 line for final processing
source("~/Repos/advent2023/rAnswers/functions/expand_seeds.R")
source("~/Repos/advent2023/rAnswers/functions/seed_to_location.R")

# Get seeds as vector
seeds <- str_extract_all(input[1], "\\d+") %>%
  unlist() %>%
  as.numeric()


current_seeds <- sort(seeds)
all_maps_df <- tibble(
  "source" = numeric(),
  "destination" = numeric(),
  "range" = numeric()
)

complete_seeds <- expand_seeds(seeds)

# sort list
complete_seeds <- complete_seeds[order(sapply(complete_seeds,head,1))]

conversion_vector <- vector(mode = "list", length = length(complete_seeds))


# start processing

for (line in input[3:length(input)]) {
  # gc()
  if (str_starts(line, "\\D")) {
    map_type <-
      str_extract(line, "\\w+-\\w+-\\w+") # if line is text, set map_type
  } else if (line == "") {
    all_maps_df <- all_maps_df |>
      arrange(all_maps_df, source)
    
    
    current_seed_index <- 1
    
    
    
    for (i in seq(1:length(all_maps_df$source))) {
      #if last value in seed range is less than next source value on map...
      if (complete_seeds[[current_seed_index]][[length(complete_seeds[[current_seed_index]])]] < all_maps_df$source[i]){
        # then all values can just be added to next list
        conversion_vector[[current_seed_index]] <- complete_seeds[[current_seed_index]]
      } else  ## ugh confusing needa  break
      
      
      
      
      if (current_seeds[current_seed_index] < all_maps_df$source[i]) {
        # move to conversion_vector as is
        
        seed_less_than <- TRUE
        while(seed_less_than == TRUE){
          conversion_vector[current_seed_index] <- current_seeds[current_seed_index]
          current_seed_index <- current_seed_index + 1  
          
          if (current_seed_index > length(current_seeds)){
            break
          } else if (current_seeds[current_seed_index] >= all_maps_df$source[i]){
            seed_less_than <- FALSE
          }
          
        }
        
      } 
      
      if (current_seeds[current_seed_index] >= all_maps_df$source[i]) {
        #if seed is equal to or greater than next source value...
        # check if in next range
        if (current_seeds[current_seed_index] >= all_maps_df$source[i] &
            current_seeds[current_seed_index] <= all_maps_df$source[i] + (all_maps_df$range[i] - 1)) {
          in_range <- TRUE
          #expand vectors - but i suppose the most efficient option here, would be to not expand the vectors, and just do the math... 
          # source_expanded = seq(all_maps_df$source[i],
          #                       all_maps_df$source[i] + (all_maps_df$range[i] - 1))
          # destination_expanded = seq(
          #   all_maps_df$destination[i],
          #   all_maps_df$destination[i] + (all_maps_df$range[i] - 1)
          # )
          while (in_range == TRUE) { #secondary loop to continue working through seeds for a given range 
            difference <- current_seeds[current_seed_index] - all_maps_df$source[i]
            conversion_vector[current_seed_index] <- all_maps_df$destination[i] + difference
            # 
            # found_seed_i <-
            #   which(source_expanded == current_seeds[current_seed_index])
            # conversion_vector[current_seed_index] <-
            #   destination_expanded[found_seed_i]
            current_seed_index <- current_seed_index + 1
            
            
            if (current_seed_index > length(current_seeds)){
              break
              print("How to exit?")
            } else if (current_seeds[current_seed_index] > all_maps_df$source[i] + (all_maps_df$range[i] - 1)) {
              in_range <- FALSE
            }
          }
          if (current_seed_index > length(current_seeds)){
            break
            print("How to exit?")
          }
          
          # if (i+1 > length(all_maps_df$source)){
          #   conversion_vector <- c(conversion_vector, current_seeds)
          # }
          
          
          
        } else {
          i <- i+1
        }
      }
      
    }
    
    
    
    #reset tibble
    all_maps_df <- tibble(
      "source" = numeric(),
      "destination" = numeric(),
      "range" = range
    )
    
    # transfer conversion vector to current_seeds for next round
    current_seeds <- sort(conversion_vector)
    
  } else {
    #populate tibble for type
    
    data_to_parse <-
      str_extract_all(line, "\\d+") %>% unlist() %>% as.numeric()  # parse destination, source, range
    destination <- data_to_parse[1]
    source = data_to_parse[2]
    range = data_to_parse[3]
    
    all_maps_df <- add_row(
      all_maps_df,
      source = source,
      destination = destination,
      range = range
    )
    
  }
  
}

min(conversion_vector)