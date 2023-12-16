seed_to_location_2<- function(starting_seed_nums, seed_range, map_names, all_maps_df) {
  conversion_steps <- vector(mode = "numeric", length = 7)
  conversion_steps[1] <- seed
  
  # loop iterates over map_names to work through conversion process
  for (m in seq_along(map_names)) {
    map_name <- map_names[m]
    
    current_chart <- filter(all_maps_df, map_type == map_name)
    
    # loop iterates through current chart type
    for (i in seq(1:length(current_chart$source))) {
      
      current_loc <- current_chart$source[i]
      current_max <- current_loc + (current_chart$range[i] - 1)
      
      # for (x in seq_along(starting_seed_nums)){
      #   seed_low <- starting_seed_nums[x]
      #   seed_high <- seed_low + (seed_range[x] - 1)
      #   if (seed_low < current_loc & seed_high > current_max){
      #     
      #   }
      #   
      # }
      # 
      
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
  
  
  
  
  
#   
#   
#   
#   
#   
#   
#   
#   
#   # filter to current tibble
#   
#   
# 
#   
#   
#   
#   soil <- filter(all_maps_df, map_type == "seed-to-soil" & source == seed) %>%
#     select(destination) %>%
#     as.numeric()  
#   if (is.na(soil)){
#     soil <- seed  
#   }
#   
#   # convert to fertilizer
#   
#   fertilizer <- filter(all_maps_df, map_type == "soil-to-fertilizer" & source == soil) %>% select(destination) %>% as.numeric()
#   if (is.na(fertilizer)){
#     fertilizer <- soil  
#   } 
#   
#   # convert to water
#   
#   water <- filter(all_maps_df, map_type == "fertilizer-to-water" & source == fertilizer) %>% 
#     select(destination) %>% 
#     as.numeric()
#   if (is.na(water)){
#     water <- fertilizer  
#   }
#   
#   # convert to light
#   
#   light <- filter(all_maps_df, map_type == "water-to-light" & source == water) %>% 
#     select(destination) %>% 
#     as.numeric()
#   if (is.na(light)){
#     light <- water  
#   }
#   
#   # convert to temp
#   temperature <- filter(all_maps_df, map_type == "light-to-temperature" & source == light) %>% 
#     select(destination) %>% 
#     as.numeric()
#   if (is.na(temperature)){
#     temperature <- light  
#   }
#   
#   # convert to humidity
#   humidity <- filter(all_maps_df, map_type == "temperature-to-humidity" & source == temperature) %>% 
#     select(destination) %>% 
#     as.numeric()
#   if (is.na(humidity)){
#     humidity <- temperature
#   } 
#   
#   # convert to location
#   location <- filter(all_maps_df, map_type == "humidity-to-location" & source == humidity) %>% 
#     select(destination) %>% 
#     as.numeric()
#   if (is.na(location)){
#     location <- humidity
#   }
#   
#   return(location)
#   
# }