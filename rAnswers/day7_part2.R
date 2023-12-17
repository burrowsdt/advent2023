########
# Part 2
# Part 1 went quickly, part 2 more frustrating. Both parts could be easily refactored and there's some hackiness for sure, but it works.

library(tidyverse)

# input <- readLines("input/day7_example_input.txt")
input <- readLines("input/day7_input.txt")

source("rAnswers/functions/determine_first_rank.R")

key <- c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "1")
values <- c("B", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "O") %>% str_to_upper() %>% rev()

hashed_values <- setNames(values, key)

#  A   K   Q   T   9   8   7   6   5   4   3   2   1 
# "O" "N" "M" "L" "K" "I" "H" "G" "F" "E" "D" "C" "B" 

hands <- vector(mode = "character", length = length(input))
bids <- vector(mode = "numeric", length = length(input))
type <- vector(mode = "character", length = length(input))
first_rank <- vector(mode = "numeric", length = length(input))
alt_hand <- vector(mode = "character", length = length(input))

# Same idea, but turn J into most powerful hand possible
for (l in seq_along(input)) {
  line_vector <- str_split_1(input[l], " ")
  hands[l] <- line_vector[1] %>%
    str_replace_all("J", "1") # get hand
  bids[l] <- as.numeric(line_vector[2]) # get bid
  
  # is there a J? if so -- determine best swap by literally just finding the best hand
  ###
  
  if (str_count(hands[l], "1") > 0 & str_count(hands[l], "1") < 5) {
    # possible replacement values to iterate over
    char_set_of_hand <-
      str_extract_all(hands[l], "[^1]") %>% unlist() %>%
      unique()
    
    temp_hands <-
      vector(mode = "character", length = length(char_set_of_hand))
    temp_type <-
      vector(mode = "character", length = length(char_set_of_hand))
    temp_rank <-
      vector(mode = "character", length = length(char_set_of_hand))
    temp_alt <-
      vector(mode = "character", length = length(char_set_of_hand))
    
    for (char in seq_along(char_set_of_hand)) {
      temp_hand <-
        str_replace_all(hands[l], "1", char_set_of_hand[char])
      # we need to generate a temp df with each iteration -- just like in part 1 -- stepping away
      char_count <- str_count(temp_hand, key)
      char_count <- char_count[which(char_count != 0)] %>%
        sort(decreasing = TRUE)
      
      type_and_rank <- determine_first_rank(char_count)
      
      temp_hands[char] <- temp_hand
      temp_type[char] <- type_and_rank[1]
      temp_rank[char] <- type_and_rank[2]
      temp_alt[char] <- str_replace_all(hands[l], hashed_values)
      
    }
    
    temp_df <- tibble(
      "hand" = temp_hands,
      "type" = temp_type,
      "first_rank" = temp_rank,
      "alt_hand" = temp_alt
    ) %>% arrange(first_rank)
    
    # use the hashed hand to sort -- win for R!
    # temp_df <- temp_df %>%
    #   group_by(first_rank) %>%
    #   arrange(alt_hand, .by_group = TRUE) %>%
    #   ungroup %>%
    #   mutate(final_rank = row_number())

    
    last_row <- temp_df %>% slice(n())
    type[l] <- last_row$type
    first_rank[l] <- last_row$first_rank
    alt_hand[l] <- str_replace_all(hands[l], hashed_values)
    
  } else {
        
    char_count <- str_count(hands[l], key)
    char_count <- char_count[which(char_count != 0)] %>%
      sort(decreasing = TRUE)
    
    type_and_rank <- determine_first_rank(char_count)
    
    type[l] <- type_and_rank[1]
    first_rank[l] <- type_and_rank[2]
    #add hashed version of hand (for sorting later)
    alt_hand[l] <- str_replace_all(hands[l], hashed_values)
    
  }
    
}

# bind together hands_df
hands_df <- tibble(
  "hand" = hands,
  "alt_hand" = alt_hand,
  "bid" = bids,
  "type" = type,
  "first_rank" = as.numeric(first_rank),
) %>%
  arrange(first_rank)

hands_df <- hands_df %>%
  group_by(first_rank) %>%
  arrange(alt_hand, .by_group = TRUE) %>%
  ungroup %>%
  mutate(final_rank = row_number())

solution <- sum(hands_df$bid * hands_df$final_rank)

print(solution) #correct!