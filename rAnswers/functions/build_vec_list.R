build_vec_list <- function(l){
  temp_list <- l
  # get difference of furthest row in our list
  difference <- diff(temp_list[[length(temp_list)]])
  temp_list[[length(temp_list) + 1]] <- difference
  
  if (length(which(difference != 0))){
    
    build_vec_list(temp_list)
    
  } else {
    
    print(paste("Reached the end, last row is ", temp_list[[length(temp_list)]]))
    print(temp_list)
    
    return(temp_list)
  }
}