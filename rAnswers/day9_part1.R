# Day 9 - Part 1

library(tidyverse)

input <- readLines("input/day9_example_input.txt")
input <- readLines("input/day9_input.txt")

source("rAnswers/functions/build_vec_list.R")

extrapolations <- vector(mode = "list", length = length(input))
final_numbers <- vector(mode="numeric", length = length(input))

# diff() is our friend!

for (i in seq_along(input)){
  # Split input line into numeric elements
  num_vec <- str_split_1(input[i], " ") %>%
    as.numeric()

  # Begin list of extrapolations 
  temp_list <- list(num_vec)

  temp_list <- build_vec_list(temp_list)
  
  # Extrapolate
  
  num_to_add <- 0
  
  for (j in rev(seq_along(temp_list))){
    curr_row <- temp_list[[j]]
    curr_row <- append(curr_row, num_to_add + curr_row[length(curr_row)])
    num_to_add <- curr_row[length(curr_row)]
    
    temp_list[[j]] <- curr_row
  }
  
  # add final history to extrapolations
  extrapolations[[i]] <- temp_list
  final_numbers[i] <- num_to_add
}

sum(final_numbers) # Correct solution
