
# Part 2
# 
# # input <- readLines("~/Repos/advent2023/input/day5_example_input.txt")
# input <- readLines("~/Repos/advent2023/input/day5_input.txt")
# 
# input <- c(input, "") # add 1 line for final processing
# source("~/Repos/advent2023/rAnswers/functions/expand_seeds.R")
# source("~/Repos/advent2023/rAnswers/functions/seed_to_location.R")
# source("~/Repos/advent2023/rAnswers/functions/generate_conversion_df.R")
# 
# # Get seeds as vector and sort
# seeds <- str_extract_all(input[1], "\\d+") %>%
#   unlist() %>%
#   as.numeric() # important not to sort seeds here for part 2
# 
# all_maps_df <- generate_conversion_df(input)
# 
# # Chart names for parsing
# 
# map_names <-
#   c(
#     "seed-to-soil",
#     "soil-to-fertilizer",
#     "fertilizer-to-water",
#     "water-to-light",
#     "light-to-temperature",
#     "temperature-to-humidity"
#   )
# 
# # # Expand seed vector for ranges
# # complete_seeds <- expand_seeds(seeds)
# # 
# # # sort list
# # complete_seeds <- complete_seeds[order(sapply(complete_seeds,head,1))]
# 
# 
# starting_seed_nums <- seeds[seq(1, length(seeds), by = 2)]
# seed_range <- seeds[seq(2, length(seeds), by = 2)]
# 
# lowest_value <- c()
# 
# for (i in seq_along(starting_seed_nums)){
#   print(paste(c("working on range ", 1)))
#   
#   seed_max <- starting_seed_nums[i] + (seed_range[i] - 1)
#   
#   for (seed in seq(starting_seed_nums[i], seed_max)) {
#     
#     value <- seed_to_location(seed, map_names, all_maps_df)
#     
#     if (length(lowest_value) == 0) {
#       lowest_value <- value
#     } else if (value < lowest_value) {
#       lowest_value <- value
#     }
#     
#     
#   }
#   
#   
# }
# 
# # 
# # 
# # for (i in seq_along(complete_seeds)) {
# #   print(c(paste("Printing section ", i)))
# # 
# #   for (seed in complete_seeds[[i]]) {
# #     value <- seed_to_location(seed, map_names, all_maps_df)
# #     if (length(lowest_value) == 0) {
# #       lowest_value <- value
# #     } else if (value < lowest_value) {
# #       lowest_value <- value
# #     }
# #   }
# # }
# 
# print(lowest_value)
# 
