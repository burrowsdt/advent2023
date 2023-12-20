# Day 8

# input <- readLines("input/day8_example1_input.txt")
# input <- readLines("input/day8_example2_input.txt")
input <- readLines("input/day8_input.txt")

source("rAnswers/functions/look_up.R")
source("rAnswers/functions/scan_coordinates.R")


# parse input
directions <- input[1]  |>  str_split_1("")
coord_all <- input[3:length(input)]

start <- vector(mode = "character", length(coord_all))
left <- vector(mode = "character", length(coord_all))
right <- vector(mode = "character", length(coord_all))

for (i in seq_along(coord_all)){
  coordinates <- str_extract_all(coord_all[i], "[:alpha:]+")  |>  unlist()
  start[i] <- coordinates[1]
  left[i] <- coordinates[2]
  right[i] <- coordinates[3]
}

directions_df <- tibble(
  "start" = start,
  "left" = left,
  "right" = right
)

solution <- scan_coordinates(directions, "AAA", 1) # Correct!

# ########
# ## Part 2 --- INCOMPLETE AND DOES NOT WORK
# 
# # Computer gave up on brute force -- the internets hinted at looking at regularity of nodes reach **Z -- so that's the approach here.
# 
# # input <- readLines("input/day8_example3_input.txt")
# input <- readLines("input/day8_input.txt")
# 
# source("rAnswers/functions/look_up.R")
# 
# # Keep the same parsing for df, but grab letters and numbers.
# 
# directions <- input[1]  |>  str_split_1("")
# coord_all <- input[3:length(input)]
# 
# start <- vector(mode = "character", length(coord_all))
# left <- vector(mode = "character", length(coord_all))
# right <- vector(mode = "character", length(coord_all))
# ends_with_a <- vector(mode = "logical", length(coord_all))
# 
# for (i in seq_along(coord_all)) {
#   coordinates <-
#     str_extract_all(coord_all[i], "[:alnum:]+")  |>  unlist()
#   start[i] <- coordinates[1]
#   left[i] <- coordinates[2]
#   right[i] <- coordinates[3]
#   
#   # Mark those that end with "A"
#   ends_with_a[i] <- str_detect(coordinates[1], "A$")
# }
# 
# directions_df <- tibble(
#   "start" = start,
#   "left" = left,
#   "right" = right,
#   "ends_with_a" = ends_with_a
# )
# 
# # Isolate those that end with A into own vector -- these are starting nodes
# starting_list <- subset(directions_df, ends_with_a == TRUE, start)  |>  as_vector()
# 
# ##### Note: I think we just need to rethink the placement of the while loop, or
# ##### the period condition --- rewrite below, thinking about when we are
# ##### breaking and how we are iterating over directions vs periods
# 
# # Modify initial function to identify "**Z" stops
# scan_coordinates_for_z <- function(directions, base, steps, periods) {
#   for (node in base) {
#     p <- 0
#     steps_intervals <- vector(mode = "numeric", length = periods)
#     
#     for (i in seq_along(directions)) {
#       direction <- directions[i]
#       print(direction)
#       
#       next_coord <- look_up(direction, node)
#       
#       if (str_detect(next_coord, "Z$")) {
#         print(paste(c(
#           "Found it! Base = ", base, " and direction = ", direction
#         )))
#         print(paste(c("Total steps: ", steps)))
#         p <- p + 1
#         steps_intervals[p] <-  steps
#         steps <- 1
#         node <- next_coord
#         if (p == periods) {
#           break
#         }
#       } else if (i < length(directions)) {
#         steps <- steps + 1
#         node <- next_coord
#       } else {
#         node <- next_coord
#         steps <- steps + 1
#         steps <- scan_coordinates_for_z(directions, node, periods, steps)
#       } 
#     }
#   }
#   return(steps_intervals)
# }
# 
# 
# test <- scan_coordinates_for_z(directions, starting_list, 1, 2)







scan_coordinates_multi <- function(directions, base, steps){
  for (i in seq_along(directions)){
    direction <- directions[i]
    print(direction)
    next_coord <- vector(mode = "character", length = length(base))
    ends_with_z <- c()
    
    for (j in seq_along(base)){
      
      next_coord[j] <- look_up(direction, base[j])
      
    }

    ends_with_z <- str_detect(next_coord, "Z$")
    
    if (all(ends_with_z)) {
      print(paste(c(
        "Found it! Final coords = ", next_coord, " and direction = ", direction
      )))
      print(paste(c("Total steps: ", steps)))
      break
    } else if (i < length(directions)) {
      steps <- steps + 1
      base <- next_coord
    } else {
      base <- next_coord
      steps <- steps + 1
      steps <- scan_coordinates_multi(directions, base, steps)
    }
  }
  return(steps)
}

scan_coordinates_multi(directions, starting_list, 1)