# Day 3 - Part 2 This one has me a bit stumped. The *easiest* approach, if I
# want to keep my very hacky solution for Part 1, might be to add something that
# tracks the relevant gear associated with each part #. Look for those that share gears; do the calcs.

# This code is atrocious and includes a lot of repetition and a bit of brute
# force. But if I refactor (and I think I'd like to, for the experience) -- I'd
# probably go back and use a df/tibble for the whole thing, plus add padding to
# get rid of edge cases. A lot of this ugliness just stems from repetition.

# For ease -- what if we use a tibble.

library(tidyverse)

example_input <- readLines("~/Documents/repos/advent2023/input/day3_example_input.txt")
input <- readLines("~/Documents/repos/advent2023/input/day3_input.txt")
source("~/Documents/repos/advent2023/rAnswers/functions/symbol_finder.R")

grid <- list()
line_number_tracker = 1
digits <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
symbols <- c("!", "@", "#", "$", "%", "^", "&", "*", "\\", "/", "~", "`", ",", "+", "-", "_", "=")
parts_sum <- 0
parts_numbers <- c()
parts_and_gears <- tibble(
  "part_number" = numeric(),
  "gear_coordinate" = character()
)

# sink("./output_log.txt")

for (line in input){
  line_as_row <- str_split_1(line, "")
  grid[[line_number_tracker]] <- line_as_row
  line_number_tracker <- line_number_tracker + 1
}

# We'll go character by character and test: a) is it a number; b) are there more
# numbers immediately next to it; c)are there symbols adjacent
row_num <- 1
col_num <- 1
current_num_vector <- c()
starting_coord <- c()
ending_coord <- c()


for (row in grid){
  col_num <- 1
  for (char in row) {
    if (char %in% digits) {
      current_num_vector <- append(current_num_vector, char) # add to current_num
      print(paste("row ", row_num, " and col", col_num, "is a number"))
      if (col_num < length(grid[row])) {
        if (is.null(starting_coord)) {
          starting_coord <- c(row_num, col_num)
        }
        if (grid[[row_num]][[col_num + 1]] %in% digits) {
          col_num <- col_num + 1
          next
        }
      }
      ending_coord <- c(row_num, col_num)
      print(paste("row ", row_num, "has a number from col", starting_coord[2], "to ", col_num, " the number is ", paste(current_num_vector, collapse = "")))
      
      #do whatever we have to do with current_num
      
      parts_sum <- symbol_finder(starting_coord, ending_coord, current_num_vector, parts_sum, parts_numbers)
      
      #reset some variables
      starting_coord <- c()
      ending_coord <- c()
      current_num_vector <- c()
      
    } else if (char %in% symbols) {
      print("i'm a symbol")
    } else {
      print("i'm a blank")
    }
    col_num <- col_num + 1
  }
  row_num <- row_num + 1
}

print(parts_sum)

# for part 2 - table to get products of shared gears
powers <- parts_and_gears %>% 
  group_by(gear_coordinate) %>% 
  filter(n() > 1) %>% 
  summarise(power = prod(part_number))

print(sum(powers$power)) # final answer for part 2

# sink()