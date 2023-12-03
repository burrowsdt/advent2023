#Day 2 - Cube Conundrum
library(stringr)

# Part 1

max <- c(red=12, green=13, blue=14)
game_sum <- 0 # will hold final solution

for (line in input) {
  #every line is a game -- if one of the max values is exceeded, the game does not get added to the total
  disqualified <- FALSE
  # isolate ID
  game_info <- str_split_1(line, ":")
  game_id <- str_extract(game_info[1], "\\d+")
  # before separating rounds, we can do a general test of the numbers in the round to see if any *possibly* are disqualifying
  # if a game includes something over 14, it's auto disqualifying
  # if a game only includes things that are 12 or under, we're in the clear
  # if a game includes 13 or 14 --- then we have to take a closer look
  rounds_nums <-
    str_extract_all(game_info[2], "\\d+") %>% unlist() %>% as.integer()
  if (any(rounds_nums > 14)) {
    next
  }
  if (all(rounds_nums <= 12)) {
    game_sum <- game_sum + as.integer(game_id)
    next
  }
  # This ugly bit of code could almost certainly be refactored, or take a
  # different approach altogether? Catch all the instances that need to be
  # individually vetted.
    else { 
    rounds_nums <-
      str_extract_all(game_info[2], "\\d+") %>% unlist() %>% as.integer()
    rounds <- str_split_1(game_info[2], ";")
    for (round in rounds) {
      # separate rounds into sets of dice
      cube_sets <- str_split_1(round, ",")
      # iterate over sets and determine if disqualifying
      for (set in cube_sets) {
        color <- str_extract(set, "[:alpha:]+")
        quantity <- str_extract(set, "\\d+") %>% as.integer()
        if (quantity > max[color]) {
          disqualified = TRUE
          break
        }
      }
    }
  }
  
  if (disqualified == TRUE) {
    next
  } else {
    game_sum <- game_sum + as.integer(game_id)
  }
}

print(game_sum)

## Part 2 - Minimum and powers
power_sum <- 0 # will hold final solution


for (game in input) {
  minimum_dice <- c(red = 0,
                    green = 0,
                    blue = 0)
  game_info <- str_split_1(game, ":")
  rounds <- str_split_1(game_info[2], ";")
  for (round in rounds) {
    cube_sets <- str_split_1(round, ",")
    # iterate over sets and determine if disqualifying
    for (set in cube_sets) {
      color <- str_extract(set, "[:alpha:]+")
      quantity <- str_extract(set, "\\d+") %>% as.integer()
      if (quantity > minimum_dice[color]) {
        minimum_dice[color] <- quantity
      }
    }
  }
  
  power_sum <- power_sum + prod(minimum_dice)
}

print(power_sum)






# 
# 
#   
#   
#   # separate rounds
#   rounds <- str_split_1(game_info[2], ";")
#   # for each round:
#   for (round in rounds) {
#     colors <- str_split_1(round, ",") #separate colors
#     for (color in colors) {
#       if (str_extract(color, "[:alpha:]+") == "blue") { #if blue and higher than max, disqualify
#         if (str_extract(color, "\\d+") > max["blue"]) {
#           #Disqualified
#           disqualified <- TRUE
#           break
#         }
#       } else if (str_extract(color, "[:alpha:]+") == "green") { #if green and higher than max, disqualify
#         if (str_extract(color, "\\d+") > max["green"]) {
#           #Disqualified
#           disqualified <- TRUE
#           break
#         }
#       } else if (str_extract(color, "[:alpha:]+") == "red") { #if red and higher than max, disqualify
#         if (str_extract(color, "\\d+") > max["red"]) {
#           #Disqualified
#           disqualified <- TRUE
#           break
#         }
#       }
#     }
#   
# }
# 
# test <- example_input[1]
# #isolate ID
# disqualified = FALSE
# game_info <- str_split_1(test, ":") 
# game_id <- str_extract(game_info[1], "\\d+")
# 
# #separate rounds
# rounds <- str_split_1(game_info[2], ";")
# #for each round: 
# for (round in rounds) {
#   colors <- str_split_1(round, ",")
#   for (color in colors) {
#     if (str_extract(color, "[:alpha:]+") == "blue") {
#       if (str_extract(color, "\\d+") > max["blue"]) {
#         #Disqualified
#         disqualified <- TRUE
#         break
#       }
#     } else if (str_extract(color, "[:alpha:]+") == "green") {
#       if (str_extract(color, "\\d+") > max["green"]) {
#         #Disqualified
#         disqualified <- TRUE
#         break
#       }
#     } else if (str_extract(color, "[:alpha:]+") == "red") {
#       if (str_extract(color, "\\d+") > max["red"]) {
#         #Disqualified
#         disqualified <- TRUE
#         break
#       }
#     }
#   }
#   
# }
# 
# 
# 
