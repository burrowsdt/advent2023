# Function for Day 8 - initiates/wraps look up sequence

scan_coordinates <- function(directions, base, steps){
  for (i in seq_along(directions)){
    direction <- directions[i]
    print(direction)
    next_coord <- look_up(direction, base)
    if (next_coord == "ZZZ") {
      print(paste(c(
        "Found it! Base = ", base, " and direction = ", direction
      )))
      print(paste(c("Total steps: ", steps)))
      break
    } else if (i < length(directions)) {
      steps <- steps + 1
      base <- next_coord
    } else {
      base <- next_coord
      steps <- steps + 1
      steps <- scan_coordinates(directions, base, steps)
    }
  }
  return(steps)
}
