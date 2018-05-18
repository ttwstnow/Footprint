#Libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(plyr)
library(rgdal)
library(sp)
library(leaflet)
library(shiny)
library(shinydashboard)
library(stringr)
library(jsonlite)
library(geojsonio)
library(rgdal)
library(maps)
library(maptools)
library(forcats)
library(FactoMineR)
library(htmlwidgets)
library(lubridate)
library(shinyLP)
library(highcharter)
library(VIM)
library(ggplot2)
library(nnet)
#install.packages("udunits2")

#Load data
countries <- fread("countries.csv", encoding = "UTF-8")

colnames(countries) <- str_replace_all(colnames(countries), " ", ".")
colnames(countries)[3] <- "Population..millions."


#Cration de la variable Urbanland.Footprint
countries <- countries %>% mutate(Urbanland.Footprint=Total.Ecological.Footprint-(Cropland.Footprint+Grazing.Footprint+Forest.Footprint+Carbon.Footprint+Fish.Footprint))
countries$Urbanland.Footprint <- round(countries$Urbanland.Footprint,2)

#Cration de la biocapacit de chaque zones 
countries <- countries %>%  mutate(
  Biocapacity.cropLand = Cropland - Cropland.Footprint,
  Biocapacity.grazing = Grazing.Land - Grazing.Footprint,
  Biocapacity.forest = Forest.Land - Forest.Footprint,
  Biocapacity.fish = Fishing.Water - Fish.Footprint,
  Biocapacity.urban = Urban.Land-Urbanland.Footprint)

#GDP.per.capita en format numérique et non en dollar
countries$GDP.per.Capita <- as.numeric(gsub('[$,]', '', countries$GDP.per.Capita))

#Crtion de GDP = GDP.per.Capita * pop en millions
countries <- countries %>% mutate(GDP=GDP.per.Capita*Population..millions.)
countries$GDP <- round(countries$GDP,2)

#Cration de HDI en facteur, variable appele Human development index
countries$Human.Development.Index <- countries$HDI
#Very high human development >=0.8
#High human development >=0.7
#Medium human development >=0.550
#Low human development le reste <0.55
countries$Human.Development.Index <- cut(countries$Human.Development.Index,breaks = c(-Inf,0.55,0.7,0.8,Inf),label=c("low ","medium ","high ","very high "),right=F) 

#Cration de GDP.per.Capita par classe de revenu variable appele Income.class
countries$Income.class <- countries$GDP.per.Capita
#1ère quartile : 1524.4
#Mdian :5430.6
#3me quartile : 14522.8
countries$Income.class <- cut(countries$Income.class,breaks = c(-Inf,1524.4,5430.6,14522.8,Inf),label=c("Poor","Intermediate","Advanced","Rich"),right=F) 


#Cration de la variable de biocapacit mais gnrale pas par personne afin de calculer des biocapacits par regions ou autre...
countries <- countries %>% mutate(Biocapacity.gMha=Biocapacity.Deficit.or.Reserve*Population..millions.)
countries$Biocapacity.gMha <- round(countries$Biocapacity.gMha,2)

#le jour du depassement
countries$day <- countries$Total.Biocapacity / countries$Total.Ecological.Footprint * 365
countries$date <- as.Date(countries$day, origin="2016-01-01")


#################################################################################################
################################## STATISTIQUES DESCRIPTIVES ####################################

#################### Mise en forme de la table pour la carte

#Importation des dones de frontires
world.map <-readOGR("TM_WORLD_BORDERS-0.3.shp", 
                    layer = "TM_WORLD_BORDERS-0.3")

#Renomme les pays pour que ce soit les mmes que ceux de "World.map" ex ds World.Map: United states et dans countries : United states of america
countries$Country<-c("Afghanistan","Albania","Algeria","Angola","Antigua and Barbuda","Argentina","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Benin","Bermuda","Bhutan","Bolivia","Bosnia and Herzegovina","Botswana","Brazil","British Virgin Islands","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cape Verde","Cambodia","Cameroon","Canada","Cayman Islands","Central African Republic","Chad","Chile","China","Colombia","Comoros","Congo","Democratic Republic of the Congo","Costa Rica","Cote d'Ivoire","Croatia","Cuba","Cyprus","Czech Republic","Denmark","Djibouti","Dominica","Dominican Republic","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Finland","France","French Guiana","French Polynesia","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Grenada","Guadeloupe","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras","Hungary","India","Indonesia","Iran (Islamic Republic of)","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Korea, Democratic People's Republic of","Korea, Republic of","Kuwait","Kyrgyzstan","Lao People's Democratic Republic","Latvia","Lebanon","Lesotho","Liberia","Libyan Arab Jamahiriya","Lithuania","Luxembourg","The former Yugoslav Republic of Macedonia","Madagascar","Malawi","Malaysia","Mali","Martinique","Mauritania","Mauritius","Mexico","Republic of Moldova","Mongolia","Montenegro","Montserrat","Morocco","Mozambique","Burma","Namibia","Nauru","Nepal","Netherlands","New Caledonia","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman","Pakistan","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Reunion","Romania","Russia","Rwanda","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Sierra Leone","Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","Spain","Sri Lanka","Suriname","Swaziland","Sweden","Switzerland","Syrian Arab Republic","Tajikistan","United Republic of Tanzania","Thailand","Timor-Leste","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Uganda","Ukraine","United Kingdom","United Arab Emirates","United States","Uruguay","Uzbekistan","Venezuela","Viet Nam","Wallis and Futuna Islands","Yemen","Zambia","Zimbabwe")
countries$Country <- as.factor(countries$Country)

world.map$Country <- world.map$NAME

countries1 <- subset(world.map@data, world.map@data$NAME %in% levels(countries$Country))

#Fusion des 2 tables
countries2 <- merge(countries, countries1, by="Country")

################ Cration de nouvelles variables pour les cartes

#Biocapacit par Rgions
# => sum(Biocapacity.gMha)/sum(Population..millions) pour chaque rgions

biocap.gMha.region<- ddply(countries2, "Region", summarise,biocap.gMha.region= sum(Biocapacity.gMha))

pop.millions.region <-ddply(countries2, "Region", summarise,pop.millions.region= sum(Population..millions.))

biocap.by.region <- merge(biocap.gMha.region,pop.millions.region,"Region")

biocap.by.region<- biocap.by.region  %>% mutate(Biocapacity.per.capita.region=biocap.gMha.region/pop.millions.region) 

countries2 <- merge(countries2,biocap.by.region,"Region")
countries2$Biocapacity.per.capita.region <- round(countries2$Biocapacity.per.capita.region,2)


#Biocapacit par HDI 
# => sum(Biocapacity.gMha)/sum(Population..millions) pour chaque niveaux de HDI
biocap.gMha.HDI <- ddply(countries2, "Human.Development.Index", summarise,biocap.gMha.HDI= sum(Biocapacity.gMha))

pop.millions.HDI <-ddply(countries2, "Human.Development.Index", summarise,pop.millions.HDI= sum(Population..millions.))

biocap.by.HDI  <- merge(biocap.gMha.HDI,pop.millions.HDI,"Human.Development.Index")

biocap.by.HDI <- biocap.by.HDI %>% mutate(Biocapacity.per.capita.HDI=biocap.gMha.HDI/pop.millions.HDI) 

countries2 <- merge(countries2,biocap.by.HDI,"Human.Development.Index")
countries2$Biocapacity.per.capita.HDI <- round(countries2$Biocapacity.per.capita.HDI,2)

#remplacer 0.27 par NA
countries2$Biocapacity.per.capita.HDI[countries2$Biocapacity.per.capita.HDI == 0.27] <- NA


#Biocapacit par GDP.per.Capita=Income.class 
# => sum(Biocapacity.gMha)/sum(Population..millions) pour chaque niveaux de GDP.per.Capita
biocap.gMha.Income <- ddply(countries2, "Income.class", summarise,biocap.gMha.Income= sum(Biocapacity.gMha))

pop.millions.Income <-ddply(countries2, "Income.class", summarise,pop.millions.Income= sum(Population..millions.))

biocap.by.Income  <- merge(biocap.gMha.Income,pop.millions.Income,"Income.class")

biocap.by.Income <- biocap.by.Income %>% mutate(Biocapacity.per.capita.Income=biocap.gMha.Income/pop.millions.Income) 

countries2 <- merge(countries2,biocap.by.Income,"Income.class")
countries2$Biocapacity.per.capita.Income <- round(countries2$Biocapacity.per.capita.Income,2)

#remplacer -0.23 par des NA
countries2$Biocapacity.per.capita.Income[countries2$Biocapacity.per.capita.Income == -0.23] <- NA

#Creation d'une variable Number.Countries 
countries2$Number.Countries <- round(countries2$Countries.Required,0)
countries2$Number.Countries <- cut(countries2$Number.Countries,breaks = c(-Inf,1,2,3,4,5,Inf),right=F,label=c("<1","1","2","3","4",">=5"))


#Creation de la variable biocapacity en "deficit" "reserve"
countries2$Biocap.Deficit.or.Reserve <- countries2$Biocapacity.Deficit.or.Reserve 
countries2$Biocap.Deficit.or.Reserve  <- cut(countries2$Biocap.Deficit.or.Reserve,breaks = c(-Inf,0,Inf),right=F,label=c("deficit ","reserve"))

#Creation de la variable Cropland biocapacity en "deficit" "reserve"
countries2$Biocap.cropland <- countries2$Biocapacity.cropLand
countries2$Biocap.cropland <- cut(countries2$Biocap.cropland,breaks = c(-Inf,0,Inf),right=F,label=c("deficit ","reserve"))

#Creation de la variable Grazing biocapacity en "deficit" "reserve"
countries2$Biocap.grazing <- countries2$Biocapacity.grazing
countries2$Biocap.grazing <- cut(countries2$Biocap.grazing,breaks = c(-Inf,0,Inf),right=F,label=c("deficit ","reserve"))

#Creation de la variable Forest biocapacity en "deficit" "reserve"
countries2$Biocap.forest <- countries2$Biocapacity.forest
countries2$Biocap.forest <- cut(countries2$Biocap.forest,breaks = c(-Inf,0,Inf),right=F,label=c("deficit ","reserve"))

#Creation de la variable Fish biocapacity en "deficit" "reserve"
countries2$Biocap.fish <- countries2$Biocapacity.fish
countries2$Biocap.fish <- cut(countries2$Biocap.fish,breaks = c(-Inf,0,Inf),right=F,label=c("deficit ","reserve"))

#Creation de la variable Urban biocapacity en "deficit" "reserve"
countries2$Biocap.urban <- countries2$Biocapacity.urban
countries2$Biocap.urban <- cut(countries2$Biocap.urban,breaks = c(-Inf,0,Inf),right=F,label=c("deficit ","reserve"))

world <- subset(world.map, world.map$NAME %in% levels(countries$Country))
world <- world[order(world$NAME),] 

countries2 <- countries2[order(countries2$Country),]

############################ MAPS labels
#accueil
labelsaccueil <- sprintf(
  "<strong>%s</strong><br/>Biocapacity: %g</strong><br/>%s",countries2$Country,countries2$Biocapacity.Deficit.or.Reserve,countries2$date
) %>% lapply(htmltools::HTML)

#population 
labels <- sprintf(
  "<strong>%s</strong><br/>%g millions",
  countries2$Country,countries2$Population..millions.
) %>% lapply(htmltools::HTML)

#Earths required
labels1 <- sprintf(
  "<strong>%s</strong><br/>%g Earths Required",
  countries2$Country,countries2$Earths.Required
) %>% lapply(htmltools::HTML)

#Countries required
labels2 <- sprintf(
  "<strong>%s</strong><br/>%g Countries Required",
  countries2$Country,countries2$Countries.Required
) %>% lapply(htmltools::HTML)


#loading data for modelling

dat <- read.table(file = "data.csv", sep = ' ', header = TRUE)


#######################################################################################################################
###############################################    server    ##########################################################
#######################################################################################################################

server <- function(input, output, session) {
  
  output$accueil <- renderLeaflet({
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color="RdYlGn", #color c est pour la couleur des frontieres
                  weight=0.5, #plus c est petit plus les traits des frontieres sont petites
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity=1),
                  smoothFactor = 0.5,
                  label=labelsaccueil,
                  opacity = 0.5, #opacite des frontieres
                  fillOpacity = 0.5, #opacite de la couleur des pays 
                  fillColor = ~colorBin("RdYlGn",countries2$Biocapicty.Deficit.or.Reserve)(countries2$Biocapacity.Deficit.or.Reserve)) %>% 
      addLegend("bottomright",
                pal = colorBin("RdYlGn",countries2$Biocapacity.Deficit.or.Reserve),
                values = countries2$Biocapacity.Deficit.or.Reserve,
                title = "Biocapacity",
                opacity = 0.5)
  })
  
  # Maps : Onglet 1
  #Population    
  output$mymap <- renderLeaflet({
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color="RdBu",
                  weight=0.5, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity = 1),
                  smoothFactor = 0.5,
                  label=labels,
                  opacity = 0.5,
                  fillOpacity = 0.5, 
                  fillColor = ~colorNumeric("RdBu",countries2$Population..millions.)(countries2$Population..millions.)) %>%  
      addLegend("bottomright",
                pal = colorNumeric("RdBu",countries2$Population..millions.),
                values = countries2$Population..millions.,
                title = "Population (millions)",
                opacity = 0.5)
  })
  
  #Human Development Index    
  output$mymap2 <- renderLeaflet({
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color="YlGnBu",
                  weight=0.5, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity = 1),
                  smoothFactor = 0.5,
                  label=countries2$Country,
                  opacity = 0.5, 
                  fillOpacity = 0.5, 
                  fillColor = ~colorFactor("YlGnBu",countries2$Human.Development.Index)(countries2$Human.Development.Index)) %>%  
      addLegend("bottomright",
                pal = colorFactor("YlGnBu",countries2$Human.Development.Index),
                values = countries2$Human.Development.Index,
                title = "Human Development Index",
                opacity = 0.5)
  })
  
  #Income class  
  output$mymap3 <- renderLeaflet({
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color="YlOrRd",
                  weight=0.5, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity = 1),
                  smoothFactor = 0.5,
                  label=countries2$Country,
                  opacity = 0.5, 
                  fillOpacity = 0.5, 
                  fillColor = ~colorFactor("YlOrRd",countries2$Income.class)(countries2$Income.class)) %>%  
      addLegend("bottomright",
                pal = colorFactor("YlOrRd",countries2$Income.class),
                values = countries2$Income.class,
                title = "Income class",
                opacity = 0.5)
  })
  
  # Maps : Onglet 2
  #Biocapacity 
  inputdata <- reactive({
    switch(input$input_biocap,
           "General biocapacity (per capita)" =countries2$Biocap.Deficit.or.Reserve ,
           "Cropland biocapacity"=countries2$Biocap.cropland ,
           "Grazing biocapacity"=countries2$Biocap.grazing,
           "Forest Biocapacity"=countries2$Biocap.forest,
           "Fish Biocapacity"=countries2$Biocap.fish,
           "Urban Biocapacity"=countries2$Biocap.urban)
  })
  
  output$mymap4 <- renderLeaflet({ 
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color=c("red2","forestgreen"),
                  weight=0.5, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity=1),
                  smoothFactor = 0.5,
                  label=countries2$Country,
                  opacity = 0.5, 
                  fillOpacity = 0.5, 
                  fillColor = ~colorFactor(c("red2","forestgreen"),inputdata()) (inputdata())) %>% 
      addLegend("bottomright",
                pal = colorFactor(c("red2","forestgreen"),inputdata()),
                values =inputdata(),
                opacity = 0.5)  
  })
  
  inputdata2 <- reactive({
    switch(input$input_biocap2,
           "By Region" =countries2$Biocapacity.per.capita.region,
           "By HDI"=countries2$Biocapacity.per.capita.HDI,
           "By Income class"=countries2$Biocapacity.per.capita.Income)
  })
  
  inputdata3 <- reactive({
    switch(input$input_biocap3,
           "Country" =countries2$Country ,
           "Region"=countries2$Region ,
           "HDI"=countries2$Human.Development.Index,
           "Income class"=countries2$Income.class)
  })
  
  output$mymap5 <- renderLeaflet({ 
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color="RdYlGn",
                  weight=0.5, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity=1),
                  smoothFactor = 0.5,
                  label=inputdata3(),
                  opacity = 0.5, 
                  fillOpacity = 0.5, 
                  fillColor = ~colorFactor("RdYlGn",inputdata2())(inputdata2())) %>%  
      addLegend("bottomright",
                pal = colorFactor("RdYlGn",inputdata2()),
                values = inputdata2(),
                title="Level of biocapacity",
                opacity = 0.5)
  })
  
  # Maps : Onglet 3
  #Earths required
  output$mymap6 <- renderLeaflet({ 
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color="YlOrRd",
                  weight=0.5, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity = 1),
                  smoothFactor = 0.5,
                  label=labels1,
                  opacity = 0.5, 
                  fillOpacity = 0.5,
                  fillColor = ~colorNumeric("YlOrRd",countries2$Earths.Required)(countries2$Earths.Required)) %>%  
      addLegend("bottomright",
                pal = colorNumeric("YlOrRd",countries2$Earths.Required),
                values = countries2$Earths.Required,
                title = "Earths Required",
                opacity = 0.5)  
  })
  
  #Countries required
  output$mymap7 <- renderLeaflet({ 
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color="YlOrRd",
                  weight=0.5, 
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity = 1),
                  smoothFactor = 0.5,
                  label=labels2,
                  opacity = 0.5, 
                  fillOpacity = 0.5, 
                  fillColor = ~colorFactor("YlOrRd",countries2$Number.Countries)(countries2$Number.Countries)) %>%  
      addLegend("bottomright",
                pal = colorFactor("YlOrRd",countries2$Number.Countries),
                values = countries2$Number.Countries,
                title = "Countries Required",
                opacity = 0.5)
  })
  
  
  
  
  
  ## Modelling
  #Linear regression
  output$other_val_show<-renderPrint({
    input$x_variables
    input$y_variable
    countries <- as.data.frame(dat)
    
    form <- sprintf("%s~%s", input$y_variable, paste0(input$x_variables, collapse="+"))
    print(form)
    
    linmodel <- lm(as.formula(form),
                   data=countries)  
    print(summary(linmodel))
  })  
  
  
  #Logistic regression
  output$other_val_show2<-renderPrint({
    input$x_variables2
    input$y_variable2
    countries <- as.data.frame(dat)
    
    form <- sprintf("%s~%s", input$y_variable2, paste0(input$x_variables2, collapse="+"))
    print(form)
    
    
    logreg <- glm(as.formula(form), 
                  data=countries,
                  family="binomial")
    print(summary(logreg))
  }) 
  
  
  #Multinomial regression
  output$other_val_show3<-renderPrint({
    input$x_variables3
    input$y_variable3
    countries <- as.data.frame(dat)
    
    form <- sprintf("%s~%s", input$y_variable3, paste0(input$x_variables3, collapse="+"))
    print(form)
    
    multinomodel <- multinom(as.formula(form),
                             data=countries,
                             MaxNWts =2000)  
    print(summary(multinomodel))
    
    
  }) 
  
}
