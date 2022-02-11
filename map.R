library(leaflet)
library(geojsonio)
library(PBSmapping)
library(rgdal)
library(raster)

# While tiles must be in the same projection as used in the leafletCRS function,
# you must always use WGS 84 longitude/latitude data for markers, circles, 
# polygons, and lines. Leaflet will automatically project the 
# coordinates when displaying.

#statesdf_list <- Get_states_function(All_events, Stationdata1)

Movements_df <- get_movements_function(combined_events_stations)

# Movements1 <- Movements %>%
#   mutate(X = as.numeric(UTM_X),
#          Y = as.numeric(UTM_Y))
# 
# attr(Movements1, "zone") = "13"
# attr(Movements1, "projection") = "UTM"
# attr(Movements1, "datum") = "GRS80"
# 
# # need a column that has x and Y for this 
# x <- convUL(Movements1, km=FALSE, southern=NULL) #colorado is in utm zone 13

Movements1 <- Movements_df %>%
  filter(
    TAG == "230000228836",
    movement_only %in% c("No Movement")
  )
  # mutate(marker_color = case_when(movement_only == "No Movement" ~ "black",
  #                                 movement_only == "Upstream Movement" ~ "green",
  #                                 movement_only == "Downstream Movement" ~ "red",
  #                                 str_detect(movement_only, "Initial Release") ~ "blue"),
  #        icon_color = case_when(str_detect(det_type, "Stationary Antenna") ~ "orange",
  #                               str_detect(det_type, "Biomark Antenna") ~ "yellow",
  #                               str_detect(det_type, "Mobile Run") ~ "purple",
  #                               str_detect(det_type, "Release") ~ "cyan",
  #                               str_detect(det_type, "Recapture") ~ "brown",
  #        )) %>%
  
# 

#this works



# getColor <- function(x1) {
#   sapply(x1$movement_only, function(movement_only) {
#     if(movement_only == "No Movement") {
#       "blue"
#     } else if(movement_only = "Upstream Movement") {
#       "green"
#     } else {
#       "red"
#     } })
# }
#MARKERCOLOR options:
  
#red, darkred, orange, green, darkgreen, blue, purple, darkpurple, cadetblue

#iconcolor can take any value

icons <- awesomeIcons(
  icon = 'flag',
  iconColor = Movements1$icon_color,
  library = 'ion',
  markerColor =  Movements1$marker_color #"lightblue"
)




leaflet(Movements1) %>%
  #filter(Movements1, TAG == "230000224079") %>%
  addProviderTiles(providers$Esri.WorldImagery,options = providerTileOptions()) %>%
  addAwesomeMarkers(
    clusterOptions = markerClusterOptions(),
    lng=~X, lat = ~Y, icon = icons,
    label = paste(Movements1$movement_only, "\n",
                  Movements1$Date),
    popup = paste(
                  "TAG:", Movements1$TAG, "<br>",
      "Release Site:", Movements1$ReleaseSite, "<br>",
                  "Detection Event:", Movements1$det_type, "<br>",
                  "Date:", as.character(Movements1$Datetime)))

###### 
x11 <- x %>%
  distinct(X, Y, .keep_all = TRUE)
  
# utms <- Movements1 %>%
#   ungroup() %>%
#   dplyr::select(X, Y)
# 
# 
# coordinates(utms) <- ~X + Y
# 
# proj4string(utms) <-  CRS("+proj=utm +zone=13 +datum=NAD83")
# 
# utm_longlat <- spTransform(utms, CRS("+proj=longlat"))
# 
# utm_longlat %>%
#   leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery,options = providerTileOptions(maxZoom = 26)) %>%
#   addMarkers(clusterOptions = markerClusterOptions(),
#              lat = ~X,
#              lng = ~Y,
#   )
  

coordinates(Movements1) <- ~ X + Y

crs(Movements1) <- CRS("+proj=utm +zone=13 +datum=WGS84") #+init=epsg:2957

#Movements11 <- spTransform(Movements1, crs("+datum=epsg:2957"))
#need to already have a crs in order to transform
Movements1 <- spTransform(Movements1, CRS("+init=epsg:2957")) #4326

x1 <- x11 %>%
  mutate(X1 = as.numeric(UTM_X),
         Y1 = as.numeric(UTM_Y))

coordinates(x1) <- c("X1", "Y1")
proj4string(x1) <-  CRS("+proj=utm +zone=13 +datum=WGS84")
x1 <- spTransform(x1, CRS("+init=epsg:4326")) #4326



x1 %>%
  leaflet() %>%
  #addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery,options = providerTileOptions(maxZoom = 26)) %>%
  #setView(-106.319856,39.165100, 15) %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             lat = ~x1$X,
             lng = ~x1$Y,
             popup = paste("Lat", x1$X, "<br>",
                           "Long", x1$Y, "<br>"),
             label = ~TAG
             )
  #addTiles(options = providerTileOptions(maxZoom = 26)) %>%
  #


library(rvest)
library(tidyverse)
# volcano.url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Like&query_0=&op_8=eq&v_8=&type_10=EXACT&query_10=None+Selected&le_2=&ge_3=&le_3=&ge_2=&op_5=eq&v_5=&op_6=eq&v_6=&op_7=eq&v_7=&t=102557&s=5&d=5"
# 
# volcano.data <- volcano.url %>%
#   read_html() %>%
#   html_nodes("table") %>% #says that we're looking for table in the url
#   .[[3]] %>% #gets 3rd element found by matching up nodeset
#   html_table()
# 
# View(volcano.data)


####bringing in .shp files

library()
#fuctnion to convert .shp files to .rds ones, which are much quicker to read into leaflet
#converts coordinate systems as well
#keep argument is a percentage/porportion of original file; so .1 would keep 10% of file
#renames file "simple"+new layer name as .rds file. Doesn't overwrite .shp file
library(rmapshaper)
layer_location <- file.path("./gis/")
name_of_layer <- "Biomark_1"
amount_to_keep <- 1
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

stationary_antennas <- readOGR(dsn = layer_location, layer = "stationary_points")
stationary_antennas <- sp::spTransform(stationary_antennas, CRS("+init=epsg:4326"))

stream_centerline <- readOGR(dsn = layer_location, layer = "stream_centerline")
stream_centerline <- sp::spTransform(stream_centerline, CRS("+init=epsg:4326"))

releasesites <- readOGR(dsn = layer_location, layer = "ReleaseSites2021")
releasesites <- sp::spTransform(releasesites, CRS("+init=epsg:4326"))


mobile_reaches <- readOGR(dsn = layer_location, layer = "mobile_reaches")
mobile_reaches <- sp::spTransform(mobile_reaches, CRS("+init=epsg:4326"))

stations_10m <- readOGR(dsn = layer_location, layer = "stations_10m")
stations_10m <- sp::spTransform(stations_10m, CRS("+init=epsg:4326"))
simple_stations1 <- ms_simplify(stations_10m, keep = .1)
write_rds(simple_stations1, file = file.path(paste0(layer_location,"/"), paste0("simple_stations.rds")))

###no need to change files other than stations to .rds becuase the others aren't slow to bring in and convert to correct coordinate system

simple_stations2 <- read_rds(file.path("./gis/simple_stations.rds"))

Station_icons <- awesomeIcons(
  icon = 'add',
  iconColor = 'black',
  library = 'ion',
  #iconHeight = 20,
  markerColor = "purple"
)

release_icons <- awesomeIcons(
  icon = 'add',
  iconColor = 'black',
  library = 'ion',
  #iconHeight = 20,
  markerColor = "white"
)



leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   options = providerTileOptions(maxZoom = 19.5)
                   ) %>%
  addAwesomeMarkers(data = stationary_antennas@coords,
             icon = Station_icons,
             clusterOptions = markerClusterOptions(),
             label = paste(stationary_antennas@data$SiteLabel),
             popup = paste(stationary_antennas@data$SiteName, "<br>",
                           "Channel Width:", stationary_antennas@data$ChannelWid, "feet"),
             group = "Antennas") %>% # error: don't know jow to get path Data from x....solved by specifying coordinate location with @ within data
  addPolylines(data = stream_centerline@lines[[1]], 
               color = "blue",
               opacity = 1,
               popup = paste("Colorado River Centerline"),
               group = "Stream Centerlines") %>%
  addPolylines(data = stream_centerline@lines[[2]],
               color = "blue",
               opacity = 1,
               popup = paste("Fraser River Centerline"),
               group = "Stream Centerlines") %>%
  addPolylines(data = mobile_reaches,
               opacity = 1,
               color = "Yellow",
               label = mobile_reaches@data$River,
               popup = paste("Mobile Run:", mobile_reaches@data$River, 
                             "<br>"),
               group = "Mobile Reaches") %>%
  addAwesomeMarkers(data = releasesites@coords,
                    icon = release_icons,
                    clusterOptions = markerClusterOptions(),
             label = releasesites@data$ReleaseSit, 
             popup = paste("Release Date1:", releasesites@data$ReleaseDat, "<br>","Release Date 2:",  releasesites@data$ReleaseD_1),
             group = "Release Sites") %>%
  addPolylines(data = simple_stations2, 
               label = simple_stations2@data$ET_STATION,
               labelOptions = labelOptions(noHide = T, textOnly = TRUE),
               group = "Stations (m)") %>%
  addLayersControl(overlayGroups = c("Antennas", "Release Sites", "Stream Centerlines", "Stations (m)", "Mobile Reaches")) %>%
  hideGroup(c("Stream Centerlines", "Stations (m)", "Mobile Reaches"))
  

  #addP

