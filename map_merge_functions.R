# This script are functions used to merge regions together whose populations are below a threshold. The steps are find the regions that are 
# neighbouring and then for each region if the population is below a threshold, combine geometry with a neighbouring region until the threshold is reached 


# region_df: sf dataframe containing polygon geometries
# region_col: name of column containing region names
# geometry_col: name of geometry column
# boundry list: List of touching boundaries returned from find_boundaries()
# names_list: list of region names
#
# find_boundries() returns:
# list where:
#   names = region names
#   values = indexes of neighbouring regions that share a border

find_boundries <- function(region_df, region_col, geometry_col){
  stopifnot(is.character(region_col), is.character(geometry_col))
  stopifnot(region_col %in% names(region_df))
  
  names_list <- unique(region_df[region_col])
  names_list <- unlist(names_list[region_col])[1:nrow(names_list)]
  names_list <- as.character(names_list)
  
  touch_list <- list()
  
  for(i in 1:nrow(region_df)){
    is_touch <- st_touches(region_df[geometry_col][i, ], region_df)
    is_touch <- lapply(is_touch, function(x) x ) 
    names(is_touch) <- paste0(names_list[i])
    touch_list <- append(touch_list, is_touch)
  }
  
  return(touch_list)
}

# Combine neighbouring ploygons until the population is above threshold

merge_boundries <- function(boundry_list, region_df, threshold, region_col, geometry_col, pop_col, names_list){
  
  stopifnot(is.numeric(threshold),
            is.character(pop_col),
            is.character(region_col),
            is.character(geometry_col))
  
  
  region_df$new_geom <- NA
  region_df$new_name <- NA
  region_df$new_pop <- NA
  
  merged_list <- c()
  
  print(merged_list)
  
  for(i in 1:length(boundry_list)){
    
    tot_pop <- c()
    pop_for_row <- as.numeric(region_df[pop_col][i, ])[1]
    target_name <- names(boundry_list)[i]
    
    if(pop_for_row >= threshold) next
    
    print(glue("{target_name} with a population of {pop_for_row} borders with: "))
    
    idx <- boundry_list[[i]]
    
    for (k in idx){
      
      target_region_name <- as.character(region_df[region_col][i, ])[1]
      border_name <- as.character(region_df[region_col][k, ])[1]
      target_region_pop <- pop_for_row
      bordered_pop <- as.numeric(region_df[pop_col][k, ])[1]
      
      bordered_geography <- st_make_valid(region_df[geometry_col][k, ][1])
      target_geography <- st_make_valid(region_df[geometry_col][i, ][1])
      
      print(glue("{names_list[k]} has a pop count of {bordered_pop}"))
      
      tot_pop <- c(tot_pop, region_df$pop[k])
      
      if(target_region_pop >= threshold) break
      if((border_name %in% merged_list) && isTRUE(region_df$new_pop[i] >= threshold)) next
      if((target_region_name %in% merged_list) && isTRUE(region_df$new_pop[i] >= threshold)) next
      
      target_region_pop <- target_region_pop + bordered_pop 
      
      region_df$new_pop[i] <- target_region_pop
      region_df$new_geom[i] <- st_as_sfc(st_union(target_geography, bordered_geography))
      region_df$new_name[i] <- paste(target_name, ";", border_name)
      
      new_tot_pop <- sum(c(tot_pop, region_df$pop[i]))
      
      print(glue("Total pop of bordered pop {new_tot_pop}"))
      
      print(glue("merged {names_list[i]} with  {names_list[k]}"))
      print(glue("{names_list[i]}, now have a population of, {region_df$new_pop[i]}"))
      
      merged_list <- append(merged_list, c(names_list[i], names_list[k]))
      
     
      
      
      if(!is.na(region_df$new_pop[i]) & region_df$new_pop[i] >= threshold) break
      
    }
    
    print(glue("---------------------------------------------------------------"))
    
  }
  
  final_merged <<- merged_list
  
  region_df <- region_df %>%
    select(!!sym(region_col), !!sym(pop_col), !!sym(geometry_col), new_name, new_geom, new_pop)
  
  
  regions_passed <- setdiff(names_list, final_merged)
  
  
  regions_passed_df <- region_df %>%
    filter(!!sym(region_col) %in% regions_passed) %>%
    mutate(new_pop = !!sym(pop_col)) %>%
    select(!!sym(region_col), !!sym(geometry_col), !!sym(pop_col))
  
  merged_regions_df <- region_df %>%
    filter(!is.na(new_geom)) %>%
    mutate(!!sym(region_col) := new_name,
           !!sym(geometry_col) := new_geom,
           !!sym(pop_col) := new_pop) %>%
    select(!!sym(region_col), !!sym(geometry_col), !!sym(pop_col))
  
  st_crs(merged_regions_df) <- 2193
  
  final_new_regions <- rbind(regions_passed_df, merged_regions_df)
  
  return(final_new_regions)
  
  
}


plot_map <- function(df, pop_col = "pop_col"){
  
  stopifnot(is.character(pop_col))
  
  plt <- ggplot(df) +
    geom_sf(aes(fill = !!sym(pop_col)), color = "darkgrey", size = 0.1) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Population") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_rect(fill = "white")
    ) 
  
 return(plt)
  
}
