# Day 1 - Advent of Code 2023
# Part 1

library(stringr)

input <- readLines("day1_input.txt")
sum_value <- 0

for (line in input){
  number_set <- str_extract_all(line, "\\d") %>% 
    unlist()
  
  new_number <- c(number_set[1], number_set[length(number_set)]) %>%
    paste(collapse = "") %>% 
    as.numeric()
  
  sum_value <- sum_value + new_number
}

print(sum_value)

# sum_value is final answer

# Part 2 - much harder than expected! 

input <- readLines("day1_input.txt")
sum_value <- 0


# Create dictionary # h/t to ursulams (https://gist.github.com/ursulams/9e79aa2f478c83da14e78751139f03c2) for this approach to taking care of the mash-ups
str_dict <- c("one" = "o1e",
              "two" = "t2o",
              "three" = "t3e",
              "four" = "f4r",
              "five" = "f5e",
              "six" = "s6x",
              "seven" = "s7n",
              "eight" = "e8t",
              "nine" = "n9e"
)

for (line in input){
  current_line <- str_replace_all(line, str_dict)
  
  number_set <- str_extract_all(current_line, "\\d") %>%
    unlist()
  
  new_number <- c(number_set[1], number_set[length(number_set)]) %>%
    paste(collapse = "") %>%
    as.numeric()
  
  sum_value <- sum_value + new_number
}
  
print(sum_value) # final answer