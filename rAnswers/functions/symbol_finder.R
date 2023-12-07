#symbol_finder
symbol_finder <- function(starting_coord,
         ending_coord,
         current_num_vector,
         parts_sum,
         parts_numbers) {
  starting_row <- as.numeric(starting_coord[[1]])
  starting_col <- as.numeric(starting_coord[[2]])
  ending_col <- as.numeric(ending_coord[[2]])
  symbol_found = FALSE
  border_case <- c("none", "none")
  
  if (starting_row == 1) {
    border_case[1] <- "top"
  }
  
  if (starting_row == length(grid)) {
    border_case[1] <- "bottom"
  }
  
  if (starting_col == 1) {
    border_case[2] <- "left"
    range <-
      seq(as.numeric(starting_coord[2]),
          as.numeric(ending_coord[2]) + 1)
  } else if (ending_col == length(grid[[1]])) {
    border_case[2] <- "right"
    range <-
      seq(as.numeric(starting_coord[2]) - 1,
          as.numeric(ending_coord[2]))
  } else {
    range <-
      seq(as.numeric(starting_coord[2]) - 1,
          as.numeric(ending_coord[2]) + 1)
  }
  
  print(paste("border case = ", border_case))
  
  # Take care of left/right independently
  print(paste("left: ", grid[[starting_row]][[range[1]]]))
  print(paste("right: ", grid[[starting_row]][[range[length(range)]]]))
  if (border_case[2] == "left") {
    if (grid[[starting_row]][[range[length(range)]]] %in% symbols) {
      print("symbol found!")
      symbol_found <- TRUE
    }
  } else if (border_case[2] == "right") {
    if (grid[[starting_row]][[range[1]]] %in% symbols) {
      print("symbol found!")
      symbol_found <- TRUE
    }
  } else if (grid[[starting_row]][[range[1]]] %in% symbols |
             grid[[starting_row]][[range[length(range)]]] %in% symbols) {
    print("symbol found!")
    symbol_found <- TRUE
  }
  
  if (symbol_found == TRUE) {
    parts_sum <-
      parts_sum + as.numeric(paste(current_num_vector, collapse = ""))
    parts_numbers <-
      append(parts_numbers, as.numeric(paste(current_num_vector, collapse = "")))
    print(symbol_found)
    return(parts_sum)
  } else {
    #otherwise search the remaining perimeter of the number
    for (coord in range) {
      if (border_case[1] == "top" | border_case[1] == "none") {
        # row + 1
        print("printing row + 1")
        print(grid[[starting_row + 1]][[coord]])
        if (grid[[starting_row + 1]][[coord]] %in% symbols == TRUE) {
          print("symbol found!")
          symbol_found <- TRUE
        }
      }
      if (border_case[1] == "bottom" | border_case[1] == "none") {
        # row - 1
        print("printing row - 1")
        print(grid[[starting_row - 1]][[coord]])
        if (grid[[starting_row - 1]][[coord]] %in% symbols == TRUE) {
          print("symbol found!")
          symbol_found <- TRUE
        }
      }
    }
  }
  if (symbol_found == TRUE) {
    parts_sum <-
      parts_sum + as.numeric(paste(current_num_vector, collapse = ""))
    parts_numbers <-
      append(parts_numbers, as.numeric(paste(current_num_vector, collapse = "")))
    print(c("Added ", paste(current_num_vector, collapse = "")))
  }
  print(symbol_found)
  return(parts_sum)
}


# A test case for troubleshooting
# # given a set of coordinates to define a number
# starting_coord <- c("10", "6")
# ending_coord <- c("10", "8")
# # determine an inclusive range for cols, including one additional on each end
#
# current_num_vector <- c(grid[[10]][[6]], grid[[10]][[7]], grid[[10]][[8]])
#
#
# print(symbol_finder(
#   starting_coord = starting_coord,
#   ending_coord = ending_coord,
#   current_num_vector = current_num_vector,
#   parts_sum = parts_sum
# )
# )
