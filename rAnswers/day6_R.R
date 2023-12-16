# Day 6 - both parts complete

library(tidyverse)

# setwd("~/Documents/repos/advent2023")
# input <- readLines("input/day6_example_input.txt")
input <- readLines("input/day6_input.txt")

source("rAnswers/functions/compute_distance.R")

races <- data.frame(
  "time" = str_extract_all(input[1], "\\d+") %>%
    unlist() %>%
    as.numeric(),
  "distance" = str_extract_all(input[2], "\\d+") %>%
    unlist() %>%
    as.numeric()
)

# Function to compute distance


all_solves <- c()

for (row in seq(1, nrow(races))){
  race_time <- races$time[row]
  goal <- races$distance[row]+1
  num_solutions <- 0
  for (charge_time in seq(1, race_time)){
    distance_travelled <- compute_distance(charge_time, race_time)  
    print(paste(c(charge_time, distance_travelled)))
    if (distance_travelled >= goal){
      num_solutions <- num_solutions + 1
    }
  }
  all_solves <- append(all_solves, num_solutions)
}

# solve part 1
print(prod(all_solves))

################
# part 2
rm(list = ls())

# input <- readLines("input/day6_example_input.txt")
input <- readLines("input/day6_input.txt")

source("rAnswers/functions/compute_distance.R")

# get time, distance numbers for single race

race_time <- str_extract_all(input[1], "\\d+") %>%
  unlist() %>%
  paste(collapse="") %>% 
  as.numeric()

distance = str_extract_all(input[2], "\\d+") %>%
  unlist() %>%
  paste(collapse="") %>% 
  as.numeric()


# We can halve the time elapsed variable and use that as a starting point for
# finding a more reasonable range. All we really need is the point at which the
# charge_time flips from too low to too high, in relation to our goal

goal <- distance + 1
charge_time <- race_time/2
result <- compute_distance(charge_time, race_time)

# find good lower boundary, starting from half and halving until result is "too low"
while (result > goal){
  print(paste(c("charge time ", charge_time, " was too high")))
  charge_time <- round(charge_time/2)
  result <- compute_distance(charge_time, race_time)
}
lower_boundary <- charge_time

result <- compute_distance(lower_boundary, race_time)

# find good upper boundary -- by increasing by 150% every iteration -- and reset
# lower_boundary, too, at the same time

while (result < goal){
  print(paste(c("charge time ", charge_time, " was too low")))
  lower_boundary <- charge_time
  charge_time <- round(charge_time*1.5)
  result <- compute_distance(charge_time, race_time)
}
upper_boundary <- charge_time

# brute force for the last bit --- to find the number that flips from too low to over the goal
# this is not great - about 8 sec on my machine (?) 

for (charge_time in seq(lower_boundary, upper_boundary)) {
  distance_travelled <- compute_distance(charge_time, race_time)
  print(paste(c(charge_time, distance_travelled)))
  if (distance_travelled >= goal) {
    flip_number <- charge_time
    print(paste(c(charge_time, " flipped the switch")))
    break
  }
}

#solution is total_numbers - kind of hacky but it works
total_solutions = (race_time/2 - flip_number) * 2 + 1