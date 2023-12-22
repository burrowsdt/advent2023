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

# for running through both nodes, use the following

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

print(solution) # solution here -- correct

# rerun but track path by adding to pre-allocated vector - only need one route
found_s <- FALSE
previous <- s_coord
present <- viable[[1]] %>% unlist()
steps <- 1
path <- vector(mode="list", length = 13684)
path[[steps]] <- present

while (!isTRUE(found_s)){
  
  print(paste("Currently on ", grid[[present[1]]][[present[2]]]))
  
  from <- determine_from(present, previous)
  to <- determine_to(from, present)  
  
  previous <- present
  
  present <- find_next(to, present)
  
  steps <- steps + 1
  
  path[[steps]] <- present
  
  if (grid[[present[1]]][[present[2]]] == "S"){
    print("Back home!")
    print(present)
    print(steps)
    
    steps_per_route <- append(steps_per_route, steps)
    
    found_s <- TRUE
    
  } 
}

##### Part 2
# So this is NOT my original idea, but I was really struggling to even begin to
# understand this problem. I thought --- geez, there's got to be a way to do
# this by thinking about the area of the loop --- but how to calculate that? 

# In looking up possible approaches, I found one shared by u/KeroTheFrog on
# reddit that introduced me to two new ideas: Shoelace formula for calculating
# the area of the loop, and Pick's Theorem --- for something, lol. I translated
# their code from Python to R below, in conjunction with my approach to part 1.
# I had to get gimmicky to complete the loop the "right" way.

# For that original Reddit post see:
# https://www.reddit.com/r/adventofcode/comments/18evyu9/comment/kcso138/

# So this is not the most "honest" win, but I count this as a personal win
# because not only did I learn a bit about geometry, I also gave in and found an
# R equivalent for destructuring assignment -- the zeallot package

library(zeallot)

sum = 0

# KeroTheFrog's original code in python
#
# for i in range(len(path)):
#   n_1 = path[i]
# n_2 = path[(i+1)%len(path)]
# x_1, y_1 = n_1
# x_2, y_2 = n_2
# sum += x_1 * y_2 - y_1 * x_2
# 
# area = abs(sum/2)
# 
# print(area-len(path)/2+1)
# inc <- length(path)-1

for (j in seq_along(path)){
  n_1 <- path[[j]]
  if (j == length(path)){
    n_2 <- path[[1]]
  } else {
    n_2 <- path[[j+1]]
  }
  
  c(x_1, y_1) %<-% n_1
  c(x_2, y_2) %<-% n_2
  sum <- sum + x_1 * y_2 - y_1 * x_2
}

area = abs(sum/2)

solution <- area-length(path)/2+1

print(solution) #Correct


