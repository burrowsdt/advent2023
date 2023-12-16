expand_seeds <- function(seeds){
  starting_seed_nums <- seeds[seq(1, length(seeds), by = 2)]
  seed_range <- seeds[seq(2, length(seeds), by = 2)]
  
  complete_seeds <- vector("list", length(starting_seed_nums))
  
  for (i in seq_along(starting_seed_nums)){
    complete_seeds[[i]] <- seq(starting_seed_nums[i], starting_seed_nums[i]+seed_range[i]-1)
  }  
  
  return(complete_seeds)
}