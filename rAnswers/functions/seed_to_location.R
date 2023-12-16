seed_to_location <- function(seed, map_names, all_maps_df) {
  conversion_steps <- vector(mode = "numeric", length = 7)
  conversion_steps[1] <- seed
  
  # loop iterates over map_names to work through conversion process
  for (m in seq_along(map_names)) {
    map_name <- map_names[m]
    
    current_chart <- filter(all_maps_df, map_type == map_name)
    
    
    # loop iterates through current chart type
    for (i in seq(1:length(current_chart$source))) {
      current_loc <- current_chart$source[i]
      #if seed is less than next source value on map...
      if (seed < current_loc) {
        # then add to conversion_steps
        conversion_steps[m+1] <- seed
        seed <- conversion_steps[m+1]
        break
      }
      
      #if seed is equal to or greater than next source value...
      if (seed >= current_loc) {
        # check if in next range
        if (seed >= current_loc &
            seed <= current_loc + (current_chart$range[i] - 1)) {
          difference <-
            seed - current_loc
          conversion_steps[m + 1] <-
            current_chart$destination[i] + difference
          seed <- conversion_steps[m+1]
          break
        } else if (seed > current_loc + (current_chart$range[i] - 1)) {
          next
        }
        
        
      }
    }
    
    conversion_steps[m + 1] <- seed
    seed <- conversion_steps[m + 1]
    
  }
  return(conversion_steps[length(conversion_steps)])
}