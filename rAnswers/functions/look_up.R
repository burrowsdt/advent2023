# Function for day 8 - basic look_up function, call with scan_coordinates

look_up <- function(direction, base){
  if (direction == "L"){
    next_coord <- subset(directions_df, start == base, select = "left") %>% 
      as.character()
  } else {
    next_coord <- subset(directions_df, start == base, select = "right") %>%
      as.character()
  }
  
  return(next_coord)
}