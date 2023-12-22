
determine_from <- function(present, previous) {
  # determine direction
  if (present[2] - previous[2] == 1) {
    from <- "west"
  } else if (present[2] - previous[2] == -1) {
    from <- "east"
  } else if (present[1] - previous[1] == 1) {
    from <- "north"
  } else {
    from <- "south"
  }
  
  return(from)
  
}

determine_to <- function(from, present){
  f <- from
  # get present pipe character
  char <- grid[[present[1]]][[present[2]]]  
  to <- subset(direction_chart, from == f, select = char) %>% as.character()
  return(to)
}


find_next <- function(to, present){
  if (to == "east"){
    next_node <- c(present[1], present[2]+1)
  } else if (to == "west"){
    next_node <- c(present[1], present[2]-1)
  } else if (to == "north"){
    next_node <- c(present[1]-1, present[2])
  } else {
    next_node <- c(present[1]+1, present[2])
  }
  
  return(next_node)
  
}

# keeping the below to investigate further -- this recursive solution led to an
# "error: c stack usage..." -- so I used eventually used the while loop. But why
# such a difference? Is the depth that great?
#
# track_loop <- function(present, previous, steps){
#
#   print(paste("Currently on ", grid[[present[1]]][[present[2]]]))
#
#   from <- determine_from(present, previous)
#   to <- determine_to(from, present)
#
#   previous <- present
#
#   present <- find_next(to, present)
#
#   steps <- steps + 1
#
#   if (grid[[present[1]]][[present[2]]] == "S"){
#     print("Back home!")
#     print(present)
#     print(steps)
#
#     return(steps)
#
#     break
#
#   } else (
#     track_loop(present, previous, steps)
#   )
# }