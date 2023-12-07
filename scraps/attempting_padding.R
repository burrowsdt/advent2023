# did not use but might be useful later -- for padding a grid

library(tidyverse)

example_input <- readLines("~/Documents/repos/advent2023/input/day3_example_input.txt")

# experiment with adding padding

added_top <- c(paste(rep(".", 10), collapse = ""), example_input, paste(rep(".", 10), collapse = ""))
final <- c()

for (el in added_top) {
  padded_vector <- paste(c(".", el, "."), collapse = "")
  final <- append(final, padded_vector)
}
