library(shinydashboard)
library(shiny)
library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(leaflet)
library(jsonlite)

library(rgdal)
library(maps)
library(sp)
library(maptools)
library(forcats)
library(FactoMineR)
library(htmlwidgets)
library(lubridate)
library(shinyLP)
library(DT)
library(highcharter)
library(udunits2)


dt <- fread("countries.csv", encoding = "UTF-8")

colnames(dt) <- str_replace_all(colnames(dt), " ", "_")
dt$GDP_per_Capita <- as.numeric(gsub('[$,]', '', dt$GDP_per_Capita))

dt <- dt %>% mutate(Build_up_Footprint = Total_Ecological_Footprint - 
                      (Cropland_Footprint + Grazing_Footprint + Forest_Footprint + Carbon_Footprint + Fish_Footprint),
                    Biocapacity_gMha = Biocapacity_Deficit_or_Reserve * `Population_(millions)`,
                    Status = ifelse(Biocapacity_Deficit_or_Reserve < 0, 1, 0),
                    bio_class = ifelse(dt$Biocapacity_Deficit_or_Reserve < -1.5, 0,
                                       ifelse(dt$Biocapacity_Deficit_or_Reserve >= -1.5 & dt$Biocapacity_Deficit_or_Reserve<0 , 2,
                                              ifelse(dt$Biocapacity_Deficit_or_Reserve>=0 & dt$Biocapacity_Deficit_or_Reserve < 1.5,3,4))), 
                    GDP = GDP_per_Capita * `Population_(millions)`)

dt$Build_up_Footprint <- round(dt$Build_up_Footprint,2)

dt <- dt %>% mutate(income_class = ifelse(dt$GDP_per_Capita<1026,"Poor",
                                          ifelse(dt$GDP_per_Capita>=1026 & dt$GDP_per_Capita<4036,"Intermediate",
                                                 ifelse(dt$GDP_per_Capita>=4036 & dt$GDP_per_Capita<12475,"Advanced","Rich"))),
                    Human_development = ifelse(dt$HDI >= 0.8,"Very High",
                                               ifelse(dt$HDI >= 0.7 & dt$HDI < 0.8,"High",
                                                      ifelse(dt$HDI >= 0.55 & dt$HDI < 0.7,"Medium","Low"))))

#reorder columns
dt <- dt[c(1,2,3,4,28,5,26,27,6,7,8,9,10,22,11,12,13,14,15,16,17,18,23,24,25,19,20,21)]

#My leaflet
dt$Country<-c("Afghanistan","Albania","Algeria","Angola","Antigua and Barbuda","Argentina","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Benin","Bermuda","Bhutan","Bolivia","Bosnia and Herzegovina","Botswana","Brazil","British Virgin Islands","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cape Verde","Cambodia","Cameroon","Canada","Cayman Islands","Central African Republic","Chad","Chile","China","Colombia","Comoros","Congo","Democratic Republic of the Congo","Costa Rica","Cote d'Ivoire","Croatia","Cuba","Cyprus","Czech Republic","Denmark","Djibouti","Dominica","Dominican Republic","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Finland","France","French Guiana","French Polynesia","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Grenada","Guadeloupe","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras","Hungary","India","Indonesia","Iran (Islamic Republic of)","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Korea, Democratic People's Republic of","Korea, Republic of","Kuwait","Kyrgyzstan","Lao People's Democratic Republic","Latvia","Lebanon","Lesotho","Liberia","Libyan Arab Jamahiriya","Lithuania","Luxembourg","The former Yugoslav Republic of Macedonia","Madagascar","Malawi","Malaysia","Mali","Martinique","Mauritania","Mauritius","Mexico","Republic of Moldova","Mongolia","Montenegro","Montserrat","Morocco","Mozambique","Burma","Namibia","Nauru","Nepal","Netherlands","New Caledonia","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman","Pakistan","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Reunion","Romania","Russia","Rwanda","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Sierra Leone","Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","Spain","Sri Lanka","Suriname","Swaziland","Sweden","Switzerland","Syrian Arab Republic","Tajikistan","United Republic of Tanzania","Thailand","Timor-Leste","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Uganda","Ukraine","United Kingdom","United Arab Emirates","United States","Uruguay","Uzbekistan","Venezuela","Viet Nam","Wallis and Futuna Islands","Yemen","Zambia","Zimbabwe")

dt$Country <- as.factor(dt$Country)
world.map <-readOGR("TM_WORLD_BORDERS-0.3.shp", 
                    layer = "TM_WORLD_BORDERS-0.3")
world.map$Country <- world.map$NAME

require(sp)
dt2 <- subset(world.map@data, world.map@data$NAME %in% levels(dt$Country))
countries <- merge(dt, dt2, by="Country")


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
