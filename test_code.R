
library(tidyverse)
UAR_Hayden_PostConst_2018 %>%
  ggplot(aes(x = Station_Corr, y = Elevation_PostConst_2018)) +
  geom_line() +
  geom_point() +
  xlim(12600, 12735) +
  ylim(9221, 9229.5) +
  theme_classic() 

library(leaflet)
library(geojsonio)
library(rgdal)
library(raster)
library(rmapshaper)
library(here)
states1 <- geojson_json(states, geometry = "polygon", group = "group")


#P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
# hrr.shp <- readShapePoly("./map_files/UAR_Reddy_THL_2018_Pre_2018Stations", verbose=TRUE, proj4string=P4S.latlon)

#fuctnion to convert .shp files to .rds ones, which are much quicker to read into leaflet
#converts coordinate systems as well
#keep argument is a percentage/porportion of original file; so .1 would keep 10% of file
#renames file "simple"+new layer name as .rds file. Doesn't overwrite .shp file
convert_shp_to_rds <- function(layer_location,name_of_layer,amount_to_keep,new_name_of_layer){
  #.shp files read in a bit quicker if they're in their own folder
  layer1 <- readOGR(dsn = layer_location, layer = name_of_layer)
  #needed because leaflet expects coordinates to be in longlat
  layer1 <- sp::spTransform(layer1, CRS("+init=epsg:4326"))
  
  simple_layer1 <- ms_simplify(layer1, keep = amount_to_keep) #might be worth saving these as shape files and doing away with other ones because they're so much smaller
  #save 
  write_rds(simple_layer1, path = file.path(paste0(layer_location,"/"), paste0("simple_",new_name_of_layer,".rds")))
  
  #simple_pools <- read_rds(file.path("./app3/map_files/simple_pools.rds"))
  
}

#convert_shp_to_rds("./rpd_app/map_files", "UAR_Reddy_THL_Post_2014_Stations", .1,"reddy_2014_stations_2")
# pools <- readOGR(dsn = "./app3/map_files", layer = "UAR_AsBuilt_ResidualPoolDepth_2019")
# #needed because leaflet expects coordinates to be in longlat
# pools <- sp::spTransform(pools, CRS("+init=epsg:4326"))
# 
# simple_pools <- ms_simplify(pools, keep = .1) #might be worth saving these as shape files and doing away with other ones because they're so much smaller
# #save 
# write_rds(simple_pools, path = file.path("./app3/map_files/", "simple_pools.rds"))
# 
# simple_pools <- read_rds(file.path("./app3/map_files/simple_pools.rds"))

states %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #addTiles(options = providerTileOptions(maxZoom = 26)) %>%
  #setView(-106.319856,39.165100, 15) %>%
  addPolygons(data = simple_pools,
              label = paste(simple_pools@data$RPD_Num_20),
              labelOptions = labelOptions(noHide = T, textOnly = TRUE))
#higher maxzoom means you can zoom in more
states %>%
  leaflet() %>%
  addTiles(options = providerTileOptions(maxZoom = 26)) %>%
  #setView(-106.319856,39.165100, 15) %>%
  addPolylines(data = simplehayden2018_readin,
              weight = 10,
              label = paste(simplehayden2018_readin@data$ET_STATION),
              labelOptions = labelOptions(noHide = T, textOnly = TRUE))# %>%
              #popup = paste(shape3@data$ET_STATION)) #%>%
  #addLabelOnlyMarkers(label = ~as.character(shape3@data$ET_STATION))
  #addPopups(~Long, ~Lat, paste(shape3@data$ET_STATION))

# leaflet() %>%
#   addTiles() %>%
#   #setView(-106.315856,39.154025, 15) %>%
#   addPolylines(data = shape1,
#                weight = 12,
#                popup = ~shape1@data$ET_STATION)
  
# 
# leaflet(data = shape1) %>%
#   addTiles() %>%
#   addPolylines()




# unique(stats_data2$Year)
# 
y <- stats_data2 %>%
  filter(Pool_Num == 32)
# 
# x <- long_pro_data %>%
#   filter(Reach == "Reddy")

longpro_filtered <- x %>%
  filter(Station >= 1850)

specific <- y %>%
  filter(Year == 2014)
upstream_station  <- specific$Station_Upstream[1]



# xcorrection2014 <- upstream_station- min(y$Station_Upstream)
# 
# longpro_filtered2 <- longpro_filtered2 %>%
#   mutate(new_station = case_when(Year == 2014 ~
#       Station-xcorrection2014
#     )
#   )



get_normalized_pool_data1 <- function (station_data, long_pro_data, pool_number, reach) {
  #unique(station_data$Year)
  
  station_data_filtered <- station_data %>%
      filter(Pool_Num == pool_number)
  
  long_pro_data_filtered <- long_pro_data %>%
    filter(Reach == reach)
  
  #gets a list of all x correctors for a specific pool over the years
  xlimits_list <- list()
  for (year_entry in unique(station_data$Year)) {
    
    #get the upstream station value from a dataset that has already been filtered on Pool number and reach
    specific_year <- station_data_filtered %>%
      filter(Year == year_entry)
    
    upstream_station  <- specific_year$Station_Upstream[1]
    #print(upstream_station)
    #get a correction value for that year based off the minimum station_upstream observed for that pool for all years
    name = paste0("Station_upstream",year_entry)
    xlimits_list[[name]] <- upstream_station - min(station_data_filtered$Station_Upstream, na.rm = TRUE)
    #[year_entry]] <- upstream_station- min(y$Station_Upstream)
    
  }
  ###getting xlimits for new stations
  #this will be the starting value for the upstream station once the new station is plotted
  starting_xlim <- min(station_data_filtered$Station_Upstream, na.rm = TRUE)
  print(starting_xlim)
  #print(xlimits_list)
  most_upstream_stations <- station_data_filtered %>%
    filter(Station_Upstream == min(station_data_filtered$Station_Upstream, na.rm = TRUE))
  
  ending_xlim <- max(most_upstream_stations$Station_Downstream)
  print(ending_xlim)
  #making new df based off station correctors for each year for the one pool
  long_pro_data_filtered2 <- long_pro_data_filtered %>%
    mutate(new_station = case_when(Year == "2010_2013" ~ Station - xlimits_list[["Station_upstream2010_2013"]],
                                   Year == "2013" ~ Station - xlimits_list[["Station_upstream2013"]],
                                   Year == "2014" ~ Station - xlimits_list[["Station_upstream2014"]],
                                   Year == "2015" ~ Station - xlimits_list[["Station_upstream2015"]],
                                   Year == "2016" ~ Station - xlimits_list[["Station_upstream2016"]],
                                   Year == "2017" ~ Station - xlimits_list[["Station_upstream2017"]],
                                   Year == "2018" ~ Station - xlimits_list[["Station_upstream2018"]]
                                   
                                  )
                                ) #end of mutate
  #new df only has station data starting and ending at the new limits of that pool
  long_pro_data_filtered2 <- long_pro_data_filtered2 %>%
    filter(between(new_station, starting_xlim, ending_xlim))
  # 
  return(long_pro_data_filtered2)
                                
  
}

###f
y <- stats_data2 %>%
  filter(Pool_Num == 18)

# x <- long_pro_data %>%
#   filter(Year == "2013") %>%
#   filter(between(Station, 3075, 3150))

test2 <- get_normalized_pool_data1(stats_data2, long_pro_data, 18, "Reddy")

min(y$Station_Upstream,na.rm = TRUE)
# 
y1 <- y %>%
  filter(Station_Upstream == min(y$Station_Upstream))

max(y1$Station_Downstream)

x1 <- test2 %>%
  ggplot(aes(x = new_station, y = Elevation, group = Year)) +
  geom_line(aes(color = Year)) + 
  theme_classic()

ggplotly(x1)

x$Station_Downstream


###shaded pools

test3 <- long_pro_data %>%
  filter(Reach == "Hayden") %>%
  ggplot(aes(x = Station, y = Elevation, group = Year)) +
  geom_line(aes(color = Year))
#iterate through a list that has x0 and x1 info for all pools in a given year
#for each iteration, add a shape to the graph



p <- ggplotly(test3)

p <- layout(p, title = 'Highlighting with Rectangles',
            shapes = list(
              list(type = "rect",
                   fillcolor = "blue", line = list(color = "blue"), opacity = 1,
                   x0 = 5000, x1 = 7000, xref = "x",
                   y0 = 9250, y1 = 9300, yref = "y"),
              list(type = "rect",
                   fillcolor = "blue", line = list(color = "blue"), opacity = 1,
                   x0 = 8000, x1 = 9000, xref = "x",
                   y0 = 9250, y1 = 9300, yref = "y")
            ))
p

fig <- plot_ly(long_pro_data, x = ~Station, y = ~Elevation, color = ~Year,
               type = "scatter",
               mode = "lines")

# add shapes to the layout
fig <- layout(fig, title = 'Highlighting with Reeectangles',
              shapes = list(shapelist$shape1,
                            shapelist$shape14,
                list(type = "rect",
                     fillcolor = "blue", line = list(color = "blue"), opacity = 1,
                     x0 = 8000, x1 = 9000, xref = "x",
                     y0 = 9250, y1 = 9300, yref = "y")
                #shapelist[[shape1]]$name
              ))
                

fig
x <- stats_data2 %>%
  filter(Year == 2013)

x1 <- list(type = "rect",
     fillcolor = "blue", line = list(color = "blue"), opacity = 1,
     x0 = 5000, x1 = 7000, xref = "x",
     y0 = 9250, y1 = 9300, yref = "y")


  
# fig <- layout(fig, title = 'Highlighting with Rectangles',
#           shapes = list(
 ###reates list of shapes with different x0 and x1 attricbutes based on station data  
shapelist <- list()
for (row in 1:nrow(x)) {
  name <- paste0("shape",row)
  #print(name)
  shapelist[[name]] <- list(type = "rect",
                                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                                x0 = x$Station_Upstream[row], x1 = x$Station_Downstream[row], xref = "x",
                                y0 = 8000, y1 = 10000, yref = "y", id = x$Pool_Num[row])
                    
    
    
  print(shapelist)
}
       #      )
       # 
       # )
       #      

lapply(shapelist, print(shapelist))
#print(x$Station_Upstream[row])
fig <- plot_ly(long_pro_data, x = ~Station, y = ~Elevation, color = ~Year,
               type = "scatter",
               mode = "lines")
# fig <- fig %>% 
#   add_trace(x = ~stats_data2$Station_Upstream, y = 9500, type = 'scatter', mode = 'lines')
fig

for (each_shape in 1:length(shapelist)) {
  #print(each_shape)
  name1 <- paste0("shape",each_shape)
  #print(name1)
  
  
      fig <- layout(fig,
                    
                      shapes =  
                        shapelist[[name1]]
                      
                      #print(shapelist[[shape]]$name)
                    
      )
}
fig
###RPD data ordinations

x <- RPD_data %>%
  mutate(Structure_type1 = case_when(Structure_Type == "NN" ~ 0,
                                     Structure_Type == "WT" ~ 1,
                                     Structure_Type == "BC" ~ 2,
                                     Structure_Type == "LV" ~ 3,
                                     Structure_Type == "LBV/WT" ~ 4,
                                     Structure_Type == "LBV" ~ 5,
                                     Structure_Type == "LV/WT" ~ 6,
                                     Structure_Type == "BV" ~ 7,
                                     Structure_Type == "WT/CT" ~ 8,
                                     Structure_Type == "BV/BT" ~ 9),
         Pool_type1 = case_when(Pool_Type == "LS" ~ 0,
                                Pool_Type == "MC" ~ 1,
                                Pool_Type == "PL" ~ 2,
                                Pool_Type == "CV" ~ 3),
         Morphology_Pre1 = case_when(Morphology_Pre == "pool" ~ 0,
                                     Morphology_Pre == "riffle" ~ 1)
  )

# write.csv(x, "UAR_ResidualPoolDepthAnalysis_2020_ver4_ordinations.csv")

RPD_data %>%
  filter(Year == "2010_2013") %>%
  group_by(Structure_Type) %>%
  count(Structure_Type)



