generate_conversion_df <- function(input) {
  map_names <-
    c(
      "seed-to-soil",
      "soil-to-fertilizer",
      "fertilizer-to-water",
      "water-to-light",
      "light-to-temperature",
      "temperature-to-humidity"
    )
  all_maps_df <- tibble(
    "map_type" = character(),
    "source" = numeric(),
    "destination" = numeric(),
    "range" = numeric()
  )
  
  # all_maps_df will be the main conversion chart df
  
  number_of_drows <- length(which(str_starts(input, "\\d") == TRUE))
  
  df_list <- vector(mode = "list", length = number_of_drows)
  i <- 1
  # Parse input and populate df
  for (line in input[3:length(input)]) {
    # create all_maps_df from input
    if (str_starts(line, "\\D")) {
      map_type <-
        str_extract(line, "\\w+-\\w+-\\w+") # if line is text, set map_type
    } else if (line == "") {
      next
    } else {
      data_to_parse <-
        str_extract_all(line, "\\d+") %>% unlist() %>% as.numeric()  # parse destination, source, range
      destination <- data_to_parse[1]
      source = data_to_parse[2]
      range = data_to_parse[3]
      
      
      df_list[[i]] <- tibble(
        map_type = map_type,
        source = source,
        destination = destination,
        range = range
      )
      
      i <- i + 1
    }
  }
  
  all_maps_df <- purrr::list_rbind(df_list) |>
    group_by(map_type) |>
    arrange(map_type, source)
  
  return(all_maps_df)
  
}