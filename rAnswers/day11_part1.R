# Day 11

library(tidyverse)
library(zeallot)

# input <- readLines("input/day11_example_input_1.txt")
input <- readLines("input/day11_input.txt")

initial_grid_size <- str_length(input[1])

# decided against padding but keeping for posterity
# row_padding <- paste(rep("O", initial_grid_size), collapse = "") # row padding
# 
# input <- c(row_padding, input, row_padding) 

# parse each line into split vector, expand rows that need to be expanded, store grid as list

rows_expanded <- list()

for (i in seq_along(input)){
  row_to_test <- str_split_1(input[i], "")
  # row_to_add <- c("O", row_to_test, "O") # column padding
  rows_expanded <- append(rows_expanded, list(row_to_test))  
  
  if (all(row_to_test == ".")){
    rows_expanded <- append(rows_expanded, list(row_to_test))  
  }
}

# Get cols to expand

cols_to_expand <- c()

for (i in seq(1, length(rows_expanded[[1]]))){
  col_vec <- rbind(lapply(rows_expanded, "[[", i))
  if (all(col_vec == ".")){
    cols_to_expand <- append(cols_to_expand, i)
  }
}

# Convert to df and add cols as necessary - yes, there are better ways? but i was struggling, lol

grid <- do.call(rbind, rows_expanded) %>% 
  data.frame()

filler_dots <- rep(".", nrow(grid))

for (i in seq_along(cols_to_expand)){
  grid <- add_column(grid, filler_dots, .after=cols_to_expand[i])
  cols_to_expand[i+1:length(cols_to_expand)] <- cols_to_expand[i+1:length(cols_to_expand)]+1
}

# Rename cols
grid <- as.matrix(grid)

# Find "#" nodes

starting_nodes <- which(grid == "#", arr.ind = TRUE)

# at this point, could use `dist` to calculate
# solution <- dist(starting_nodes, method = "manhattan") %>% sum()  # correct!
# but without the shortcut... manhattan distance 

dist_tracker <- c()

# node1 <- starting_nodes[1,]
# node2 <- starting_nodes[4,]


dist_calc <- function(node1, node2){
  
  step1 <- node1 - node2
  distance <- abs(step1[1]) + abs(step1[2])
  return(as.numeric(distance))
  
}

for (i in seq(1, nrow(starting_nodes))){
  node1 <- starting_nodes[i,]
  for (j in seq(i+1, nrow(starting_nodes))){
    node2 <- starting_nodes[j,]
    dist_tracker <- append(dist_tracker, dist_calc(node1, node2))
  }
}

solution <- sum(dist_tracker)