library(tidyverse)

# input <- readLines("input/day7_example_input.txt")
input <- readLines("input/day7_input.txt")

key <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
values <- letters[1:13] %>% str_to_upper() %>% rev()

hashed_values <- setNames(values, key)
#
# hashed_values <-
#   c(
#     "2" = "A",
#     "3" = "B",
#     "4" = "C",
#     "5" = "D",
#     "6" = "E",
#     "7" = "F",
#     "8" = "G",
#     "9" = "H",
#     "T" = "I",
#     "J" = "J",
#     "Q" = "K",
#     "K" = "L",
#     "A" = "M"
#   )


hands <- vector(mode = "character", length = length(input))
bids <- vector(mode = "numeric", length = length(input))
type <- vector(mode = "character", length = length(input))
first_rank <- vector(mode = "numeric", length = length(input))
alt_hand <- vector(mode = "character", length = length(input))


# parse input into hands_df

for (l in seq_along(input)) {
  line_vector <- str_split_1(input[l], " ")
  hands[l] <- line_vector[1] # get hand
  bids[l] <- as.numeric(line_vector[2]) # get bid
  
  # parse type and assign first rank
  char_count <- str_count(line_vector[1], key)
  char_count <- char_count[which(char_count != 0)] %>%
    sort(decreasing = TRUE)
  
  if (char_count[1] == 5) {
    current_type <- "five"
    current_rank <- 7
  } else if (char_count[1] == 4) {
    current_type <- "four"
    current_rank <- 6
  } else if (char_count[1] == 3 & char_count[2] == 2) {
    current_type <- "full_house"
    current_rank <- 5
  } else if (char_count[1] == 3) {
    current_type <- "three"
    current_rank <- 4
  } else if (char_count[1] == 2 & char_count[2] == 2) {
    current_type <- "two_pair"
    current_rank <- 3
  } else if (char_count[1] == 2) {
    current_type <- "one_pair"
    current_rank <- 2
  } else {
    current_type <- "high"
    current_rank <- 1
  }
  
  type[l] <- current_type
  first_rank[l] <- current_rank
  #add hashed version of hand (for sorting later)
  alt_hand[l] <- str_replace_all(line_vector[1], hashed_values)
}

# bind together hands_df
hands_df <- tibble(
  "hand" = hands,
  "alt_hand" = alt_hand,
  "bid" = bids,
  "type" = type,
  "first_rank" = first_rank
) %>%
  arrange(first_rank)

# use the hashed hand to sort -- win for R!
hands_df <- hands_df %>%
  group_by(first_rank) %>%
  arrange(alt_hand, .by_group = TRUE) %>%
  ungroup %>%
  mutate(final_rank = row_number())

solution <- sum(hands_df$bid * hands_df$final_rank)

print(solution) #correct!