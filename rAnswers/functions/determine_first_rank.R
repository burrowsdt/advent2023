determine_first_rank <- function(char_count){
  # parse type and assign first rank
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
  
  type_and_rank <- c(current_type, current_rank)
  return(type_and_rank)
  
}