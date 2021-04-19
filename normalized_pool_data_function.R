###This function takes station dataset, long pro dataset (this has already been filtered by reach), and pool number, and normalizes the station data so that each pool will start at the same point.
#allows you to more easily see depth changes over time of an individual pool
#returns a list containing dataset with new_station variable that is noramlized for that specific pool chosen



get_normalized_pool_data <- function (station_data, long_pro_data_filtered, pool_number, xmin_corrector, xmax_corrector) {
  #unique(station_data$Year)
  
  station_data_filtered <- station_data %>%
    filter(Pool_Num == pool_number)
  
  #y_min <- min(station_data_filtered)
  
  #gets a list of all x correctors for a specific pool over the years
  x_station_correctors_list <- list()
  for (year_entry in unique(station_data$Year)) {
    
    #get the upstream station value from a dataset that has already been filtered on Pool number and reach
    specific_year <- station_data_filtered %>%
      filter(Year == year_entry)
    
    upstream_station  <- specific_year$Station_Upstream[1]
    #print(upstream_station)
    #get a correction value for that year based off the minimum station_upstream observed for that pool for all years
    name = paste0("Station_upstream",year_entry)
    x_station_correctors_list[[name]] <- upstream_station - min(station_data_filtered$Station_Upstream, na.rm = TRUE)
    #[year_entry]] <- upstream_station- min(y$Station_Upstream)
    
  }
  ###getting xlimits for new stations
  #this will be the starting value for the upstream station once the new station is plotted
  starting_xlim <- min(station_data_filtered$Station_Upstream, na.rm = TRUE) + xmin_corrector
  
  #print(x_station_correctors_list)
  most_upstream_stations <- station_data_filtered %>%
    filter(Station_Upstream == min(station_data_filtered$Station_Upstream, na.rm = TRUE))
  
  ending_xlim <- max(most_upstream_stations$Station_Downstream) + xmax_corrector
  
  #making new df based off station correctors for each year for the one pool
  long_pro_data_filtered2 <- long_pro_data_filtered %>%
    mutate(new_station = case_when(Year == "2010_2013" ~ Station - x_station_correctors_list[["Station_upstream2010_2013"]],
                                   Year == "2013" ~ Station - x_station_correctors_list[["Station_upstream2013"]],
                                   Year == "2014" ~ Station - x_station_correctors_list[["Station_upstream2014"]],
                                   Year == "2015" ~ Station - x_station_correctors_list[["Station_upstream2015"]],
                                   Year == "2016" ~ Station - x_station_correctors_list[["Station_upstream2016"]],
                                   Year == "2017" ~ Station - x_station_correctors_list[["Station_upstream2017"]],
                                   Year == "2018" ~ Station - x_station_correctors_list[["Station_upstream2018"]]
                                   
    )
    ) #end of mutate
  #new df only has data starting and ending at the new limits of that pool
  long_pro_data_filtered2 <- long_pro_data_filtered2 %>%
    filter(between(new_station, starting_xlim, ending_xlim))
  #wrap the output into a list which is easily returned and accessible; redundant when there's only one variable being returned
  #but if we decide to not filter for xlims within this function, we'd want to return those lims as part of the list
  returned_list <- list("long_pro_data_filtered2" = long_pro_data_filtered2)
  #print(returned_list)
  return(returned_list)
  
  
}