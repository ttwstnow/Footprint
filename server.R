#######################################################################################################################
###############################################    server    ##########################################################
#######################################################################################################################

server <- function(input, output, session) {
  
  #map
  names(providers)
  
  couleurs <- colorNumeric("YlOrRd", countries$Biocapacity_Deficit_or_Reserve, n = 20)
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = countries$Biocapacity_Deficit_or_Reserve)
  
  output$g1 <- renderLeaflet({
    
    countries %>% 
      select(LON,LAT) %>% 
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap) %>% 
      fitBounds(lng1 = ~min(countries$LON),  lat1 = ~min(countries$LAT), lng2 = ~max(countries$LON),  lat2 = ~max(countries$LAT)) %>% 
      addCircles(lng = ~LON, lat = ~LAT, weight = 15,
                 radius = ~countries$Biocapacity_Deficit_or_Reserve * 1000, 
                 popup = countries$Country,
                 color = ~couleurs(countries$Biocapacity_Deficit_or_Reserve), fillOpacity = 0.9 ) %>% 
      addLegend("bottomright", pal = pal, values = countries$Biocapacity_Deficit_or_Reserve, title = "Biocapacity", 
                opacity = 1)
    
  })
  
  # Add polygons for the 188 countries of my database
  world <- subset(world.map, world.map$NAME %in% levels(countries$Country))
  world <- world[order(world$NAME),] 
  
  
  output$g2 <- renderLeaflet({
    
    leaflet(world) %>% 
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color = "YlOrRd",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.5,
                  label=countries$Country,
                  fillColor = ~colorQuantile("YlOrRd", countries$Biocapacity_Deficit_or_Reserve)(countries$Biocapacity_Deficit_or_Reserve),
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE)) %>%
      addCircleMarkers(lng=countries$LON,
                       lat=countries$LAT,
                       opacity=0.1,
                       radius=0.001,
                       #label=countries$Country,
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>%  
      addLegend("bottomright",
                pal = pal,
                values = countries$Biocapacity_Deficit_or_Reserve,
                title = "Biocapacity",
                opacity = 1)
    
  })
  
}
