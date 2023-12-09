seed_to_location <- function(seed){
  # convert to soil
  soil <- filter(all_maps_df, map_type == "seed-to-soil" & source == seed) %>%
    select(destination) %>%
    as.numeric()  
  if (is.na(soil)){
    soil <- seed  
  }
  
  # convert to fertilizer
  
  fertilizer <- filter(all_maps_df, map_type == "soil-to-fertilizer" & source == soil) %>% select(destination) %>% as.numeric()
  if (is.na(fertilizer)){
    fertilizer <- soil  
  } 
  
  # convert to water
  
  water <- filter(all_maps_df, map_type == "fertilizer-to-water" & source == fertilizer) %>% 
    select(destination) %>% 
    as.numeric()
  if (is.na(water)){
    water <- fertilizer  
  }
  
  # convert to light
  
  light <- filter(all_maps_df, map_type == "water-to-light" & source == water) %>% 
    select(destination) %>% 
    as.numeric()
  if (is.na(light)){
    light <- water  
  }
  
  # convert to temp
  temperature <- filter(all_maps_df, map_type == "light-to-temperature" & source == light) %>% 
    select(destination) %>% 
    as.numeric()
  if (is.na(temperature)){
    temperature <- light  
  }
  
  # convert to humidity
  humidity <- filter(all_maps_df, map_type == "temperature-to-humidity" & source == temperature) %>% 
    select(destination) %>% 
    as.numeric()
  if (is.na(humidity)){
    humidity <- temperature
  } 
  
  # convert to location
  location <- filter(all_maps_df, map_type == "humidity-to-location" & source == humidity) %>% 
    select(destination) %>% 
    as.numeric()
  if (is.na(location)){
    location <- humidity
  }
  
  return(location)
  
}