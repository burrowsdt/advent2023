# Day 10

library(tidyverse)

# input <- readLines("input/day10_example_input.txt")
# input <- readLines("input/day10_example_input_2.txt")

input <- readLines("input/day10_input.txt")

# functions for pathfinding and traversal
source("rAnswers/functions/day10_functions.R")

# Prepping input and resources

# how wide a grid? create padding, add to input
grid_width <- str_length(input[1])
padding <- paste(rep(".", grid_width), collapse="")
input <- c(padding, input, padding)

# split to list, then vectors of characters; add padding; get in grid formation
grid <- vector(mode = "list", length(input))

for (i in seq_along(input)){ # While parsing, hunt for S!
  if (str_detect(input[i], "S")){
    s_coord_x <- as.numeric(i) 
    s_coord_y <- str_locate(input[i], "S")[1] + 1
  }
  
  char_vec <- str_split_1(input[i], "")
  char_vec <- c(".", char_vec, ".") # Add horizontal padding
  grid[[i]] <- char_vec
}

# stash s coordinate
s_coord <- c(s_coord_x, s_coord_y)

# Pathfinder - a direction chart gives possible destinations by char

direction_chart <- tibble(
  "from" = c("east", "west", "south", "north"),
  "-" = c("west", "east", NA, NA),
  "L" = c("north", NA, NA, "east"),
  "F" = c("south", NA, "east", NA),
  "J" = c(NA, "north", NA, "west"),
  "7" = c(NA, "south", "west", NA),
  "|" = c(NA, NA, "north", "south")
)

# We need to find the viable nodes around S - just check each manually
viable <- list()
steps_per_route <- c()

if (grid[[s_coord_x]][[s_coord_y + 1]] %in% c("-", "J", "7")){
  viable[[length(viable)+1]] <- c(s_coord_x, s_coord_y+1)
} 
if (grid[[s_coord_x]][[s_coord_y - 1]] %in% c("-", "L", "F")){
  viable[[length(viable)+1]] <- c(s_coord_x, s_coord_y-1)
} 
if (grid[[s_coord_x+1]][[s_coord_y]] %in% c("|", "J", "L")){
  viable[[length(viable)+1]] <- c(s_coord_x+1, s_coord_y)
} 
if (grid[[s_coord_x-1]][[s_coord_y]] %in% c("|", "F", "7")){
  viable[[length(viable)+1]] <- c(s_coord_x-1, s_coord_y)
} 


for (i in seq_along(viable)){
  found_s <- FALSE
  previous <- s_coord
  present <- viable[i] %>% unlist()
  steps <- 1
  
  while (!isTRUE(found_s)){
    
    print(paste("Currently on ", grid[[present[1]]][[present[2]]]))
    
    from <- determine_from(present, previous)
    to <- determine_to(from, present)  
    
    previous <- present
    
    present <- find_next(to, present)
    
    steps <- steps + 1
    
    if (grid[[present[1]]][[present[2]]] == "S"){
      print("Back home!")
      print(present)
      print(steps)
      
      steps_per_route <- append(steps_per_route, steps)
      
      found_s <- TRUE
      
    } 
  }  
}

if (steps_per_route[1] == steps_per_route[2]){
  solution <- steps_per_route[1]/2
}

print(solution)