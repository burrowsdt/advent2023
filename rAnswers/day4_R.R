library(tidyverse)

input <- readLines("~/Documents/repos/advent2023/input/day4_input.txt")
example_input <- readLines("~/Documents/repos/advent2023/input/day4_example_input.txt")

# model for parsing data into card #, winning numbers, and numbers on the card

card_num = 1
total_points <- 0
## Added df for part 2
df_matches <- tibble(
  "card" = numeric(),
  "matches" = numeric(),
)

for (line in input) {
  basic_split <- line %>%
    str_remove(".+:") %>%
    str_split_1("\\|")
  
  winning_nums <- basic_split[1] %>%
    str_extract_all("\\d+") %>%
    unlist() %>%
    as.numeric()
  
  current_nums <- basic_split[2] %>%
    str_extract_all("\\d+") %>%
    unlist() %>%
    as.numeric()
  
  points <- 0
  matches <- 0
  
  for (num in winning_nums) {
    
    if (num %in% current_nums & points == 0) {
      points <- 1
      matches <- matches + 1
    } else if (num %in% current_nums & points != 0) {
      points <- points * 2
      matches <- matches + 1
    }
  }
  
  total_points <- total_points + points
  df_matches <- add_row(df_matches, card = card_num, matches = matches)
  card_num <- card_num + 1
}

print(total_points) # part 1 answer

# Part 2

df_matches$copies <- 1
current_card = 1

for (r in seq_along(df_matches$matches)){
  counter <- df_matches$matches[[r]]
  i <- 1
  while (i <= counter){
    df_matches$copies[[current_card + i]] <- df_matches$copies[[current_card + i]] + df_matches$copies[[r]] 
    i <- i + 1
  }
  current_card <- current_card + 1
}

print(sum(df_matches$copies)) # part 2 answer
