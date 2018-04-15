################################################################################################
################################### Shiny ######################################################
################################################################################################
rm(list=ls())

#Libraries
library(dplyr)
library(ggplot2)
library(plyr)
library(rgdal)
library(sp)
library(leaflet)
library(shiny)
library(shinydashboard)

#Load data
countries <- read.csv("countries.csv",header=T,sep=",",encoding = "UTF-8")

#Création de nouvelles variables :

#Création de la variable Urbanland.Footprint
countries <- countries %>% mutate(Urbanland.Footprint=Total.Ecological.Footprint-(Cropland.Footprint+Grazing.Footprint+Forest.Footprint+Carbon.Footprint+Fish.Footprint))
countries$Urbanland.Footprint <- round(countries$Urbanland.Footprint,2)

#Création de la biocapacité de chaque zones 
countries <- countries %>%  mutate(
  Biocapacity.cropLand = Cropland - Cropland.Footprint,
  Biocapacity.grazing = Grazing.Land - Grazing.Footprint,
  Biocapacity.forest = Forest.Land - Forest.Footprint,
  Biocapacity.fish = Fishing.Water - Fish.Footprint,
  Biocapacity.urban = Urban.Land-Urbanland.Footprint)

#GDP.per.capita en format numérique et non en dollar
countries$GDP.per.Capita <- as.numeric(gsub('[$,]', '', countries$GDP.per.Capita))

#Création de GDP = GDP.per.Capita * pop en millions
countries <- countries %>% mutate(GDP=GDP.per.Capita*Population..millions.)
countries$GDP <- round(countries$GDP,2)

#Création de HDI en facteur, variable appelée Human development index
countries$Human.Development.Index <- countries$HDI
#Very high human development >=0.8
#High human development >=0.7
#Medium human development >=0.550
#Low human development le reste <0.55
countries$Human.Development.Index <- cut(countries$Human.Development.Index,breaks = c(-Inf,0.55,0.7,0.8,Inf),label=c("low ","medium ","high ","very high "),right=F) 

#Création de GDP.per.Capita par classe de revenu variable appelée Income.class
countries$Income.class <- countries$GDP.per.Capita
#1ère quartile : 1524.4
#Médian :5430.6
#3ème quartile : 14522.8
countries$Income.class <- cut(countries$Income.class,breaks = c(-Inf,1524.4,5430.6,14522.8,Inf),label=c("Poor","Intermediate","Advanced","Rich"),right=F) 


#Création de la variable de biocapacité mais générale pas par personne afin de calculer des biocapacités par regions ou autre...
countries <- countries %>% mutate(Biocapacity.gMha=Biocapacity.Deficit.or.Reserve*Population..millions.)
countries$Biocapacity.gMha <- round(countries$Biocapacity.gMha,2)

#le jour du depassement
countries$day <- countries$Total.Biocapacity / countries$Total.Ecological.Footprint * 365
countries$date <- as.Date(countries$day, origin="2016-01-01")


#################################################################################################
################################## STATISTIQUES DESCRIPTIVES ####################################

#################### Mise en forme de la table pour la carte

#Importation des données de frontières
world.map <-readOGR("TM_WORLD_BORDERS-0.3.shp", 
                    layer = "TM_WORLD_BORDERS-0.3")

#Renomme les pays pour que ce soit les mêmes que ceux de "World.map" ex ds World.Map: United states et dans countries : United states of america
countries$Country<-c("Afghanistan","Albania","Algeria","Angola","Antigua and Barbuda","Argentina","Armenia","Aruba","Australia","Austria","Azerbaijan","Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Benin","Bermuda","Bhutan","Bolivia","Bosnia and Herzegovina","Botswana","Brazil","British Virgin Islands","Brunei Darussalam","Bulgaria","Burkina Faso","Burundi","Cape Verde","Cambodia","Cameroon","Canada","Cayman Islands","Central African Republic","Chad","Chile","China","Colombia","Comoros","Congo","Democratic Republic of the Congo","Costa Rica","Cote d'Ivoire","Croatia","Cuba","Cyprus","Czech Republic","Denmark","Djibouti","Dominica","Dominican Republic","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Finland","France","French Guiana","French Polynesia","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Grenada","Guadeloupe","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras","Hungary","India","Indonesia","Iran (Islamic Republic of)","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Korea, Democratic People's Republic of","Korea, Republic of","Kuwait","Kyrgyzstan","Lao People's Democratic Republic","Latvia","Lebanon","Lesotho","Liberia","Libyan Arab Jamahiriya","Lithuania","Luxembourg","The former Yugoslav Republic of Macedonia","Madagascar","Malawi","Malaysia","Mali","Martinique","Mauritania","Mauritius","Mexico","Republic of Moldova","Mongolia","Montenegro","Montserrat","Morocco","Mozambique","Burma","Namibia","Nauru","Nepal","Netherlands","New Caledonia","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman","Pakistan","Panama","Papua New Guinea","Paraguay","Peru","Philippines","Poland","Portugal","Qatar","Reunion","Romania","Russia","Rwanda","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","Sao Tome and Principe","Saudi Arabia","Senegal","Serbia","Sierra Leone","Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","Spain","Sri Lanka","Suriname","Swaziland","Sweden","Switzerland","Syrian Arab Republic","Tajikistan","United Republic of Tanzania","Thailand","Timor-Leste","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Uganda","Ukraine","United Kingdom","United Arab Emirates","United States","Uruguay","Uzbekistan","Venezuela","Viet Nam","Wallis and Futuna Islands","Yemen","Zambia","Zimbabwe")
countries$Country <- as.factor(countries$Country)

world.map$Country <- world.map$NAME

countries1 <- subset(world.map@data, world.map@data$NAME %in% levels(countries$Country))

#Fusion des 2 tables
countries2 <- merge(countries, countries1, by="Country")

################ Création de nouvelles variables pour les cartes

#Biocapacité par Régions
# => sum(Biocapacity.gMha)/sum(Population..millions) pour chaque régions

biocap.gMha.region<- ddply(countries2, "Region", summarise,biocap.gMha.region= sum(Biocapacity.gMha))

pop.millions.region <-ddply(countries2, "Region", summarise,pop.millions.region= sum(Population..millions.))

biocap.by.region <- merge(biocap.gMha.region,pop.millions.region,"Region")

biocap.by.region<- biocap.by.region  %>% mutate(Biocapacity.per.capita.region=biocap.gMha.region/pop.millions.region) 

countries2 <- merge(countries2,biocap.by.region,"Region")
countries2$Biocapacity.per.capita.region <- round(countries2$Biocapacity.per.capita.region,2)


#Biocapacité par HDI 
# => sum(Biocapacity.gMha)/sum(Population..millions) pour chaque niveaux de HDI
biocap.gMha.HDI <- ddply(countries2, "Human.Development.Index", summarise,biocap.gMha.HDI= sum(Biocapacity.gMha))

pop.millions.HDI <-ddply(countries2, "Human.Development.Index", summarise,pop.millions.HDI= sum(Population..millions.))

biocap.by.HDI  <- merge(biocap.gMha.HDI,pop.millions.HDI,"Human.Development.Index")

biocap.by.HDI <- biocap.by.HDI %>% mutate(Biocapacity.per.capita.HDI=biocap.gMha.HDI/pop.millions.HDI) 

countries2 <- merge(countries2,biocap.by.HDI,"Human.Development.Index")
countries2$Biocapacity.per.capita.HDI <- round(countries2$Biocapacity.per.capita.HDI,2)

#remplacer 0.27 par NA
countries2$Biocapacity.per.capita.HDI[countries2$Biocapacity.per.capita.HDI == 0.27] <- NA


#Biocapacité par GDP.per.Capita=Income.class 
# => sum(Biocapacity.gMha)/sum(Population..millions) pour chaque niveaux de GDP.per.Capita
biocap.gMha.Income <- ddply(countries2, "Income.class", summarise,biocap.gMha.Income= sum(Biocapacity.gMha))

pop.millions.Income <-ddply(countries2, "Income.class", summarise,pop.millions.Income= sum(Population..millions.))

biocap.by.Income  <- merge(biocap.gMha.Income,pop.millions.Income,"Income.class")

biocap.by.Income <- biocap.by.Income %>% mutate(Biocapacity.per.capita.Income=biocap.gMha.Income/pop.millions.Income) 

countries2 <- merge(countries2,biocap.by.Income,"Income.class")
countries2$Biocapacity.per.capita.Income <- round(countries2$Biocapacity.per.capita.Income,2)

#remplacer -0.23 par des NA
countries2$Biocapacity.per.capita.Income[countries2$Biocapacity.per.capita.Income == -0.23] <- NA

#Création d'une variable Number.Countries 
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


###########################################################################################################
###########################################################################################################

########################################################
############################# HEADER ###################
header <-  dashboardHeader(titleWidth = 250,
dropdownMenu(messageItem(from = "Github", message = "Click for application code", href = "https://github.com/ttwstnow/Footprint.git"),
               messageItem(from = "Gobal Footprint Network", message = "Click for more informations about data", icon = icon("home"), href =  "http://data.footprintnetwork.org",  time = "10:00"),
               messageItem(from = "Kaggle", message = "Previous works on data", icon = icon("rss"), time = "2018-04-01", href =  "https://www.kaggle.com/footprintnetwork/ecological-footprint"))
)

########################################################
############################ SIDEBAR ###################
sidebar <-dashboardSidebar(width=250,
    sidebarMenu(menuItem("Welcome",tabName = "welcome",icon=icon("leaf")),
                menuItem("Explore Data",tabName = "explore", icon = icon("search"),
                  menuItem("Maps",tabName = "maps", icon = icon("globe"),
                         menuSubItem("Features", tabName = "Features", icon = icon("globe")),
                         menuSubItem("Biocapacity", tabName = "Biocapacity", icon = icon("globe")),
                         menuSubItem("Requierement", tabName = "Requierement", icon = icon("globe"))),
                  menuItem("Descriptive statistics",tabName = "stats",icon=icon("binoculars"))),         
                menuItem("Data analysis", tabName = "analyse", icon = icon("bar-chart"),
                         menuSubItem("Principal component analysis", tabName="pca"),
                         menuSubItem("Multiple correspondence analysis", tabName="mca"),
                         menuSubItem("Hierarchical clustering", tabName = "hca"),
                         menuSubItem("K-means", tabName = "hca")),
                menuItem("Modelling", tabName = "modeling", icon = icon("line-chart"),
                         menuSubItem("Regression", tabName = "reg"))
                ))

########################################################
############################ BODY ######################
body <- dashboardBody(
  
  tabItems(
  
  tabItem(tabName="welcome",
          fluidRow(box(leafletOutput("accueil"),width=12,height=500,status = "success",solidHeader = TRUE,title="Earth Overshoot Day"))),
  
  tabItem(tabName = "Features", 
          fluidRow(box(leafletOutput("mymap"),width=6, height =500,status = "success",title="World population"),
                   box(leafletOutput("mymap2"),width=6,height =500,status = "success",title="Human Development Index (HDI)"),
                   box(leafletOutput("mymap3"),width=6,height =500,status = "success",title="Income class"),
                   box(title="Definiton",width=3,height =300,background="olive","Human Development Index (HDI) : ", br(),
                                                                                "Very high >= 0.8", br(),
                                                                                "0.8 > High >= 0.7", br(),
                                                                                "0.7 > Medium >= 0.55", br(),
                                                                                "Low < 0.55", br(),br(),
                                                                                "Income class ($/per capita): ", br(),
                                                                                "Rich > 14 522.8 ", br(),
                                                                                "14 522.8 > Advanced >= 5 430.6", br(),
                                                                                "5 430.6 > Intermediate >= 1 524.4", br(),
                                                                                "Poor < 1 524.4")
          )),
  
  tabItem(tabName = "Biocapacity",
          fluidRow( column(width = 7,
                           box(selectInput(inputId="input_biocap", NULL, choices = c("General biocapacity (per capita)","Cropland biocapacity","Grazing biocapacity","Forest Biocapacity","Fish Biocapacity","Urban Biocapacity")), status = "success",title = "Choose type of biocapacity",width = NULL),
                           box(leafletOutput("mymap4"),height =500,width=NULL,status = "success",title="Map of biocapacity")),
                           box(title = "What is the biocapacity ?",width = 5,height =110, background = "olive",
                               "The biocapacity is the difference between total biocapacity (environmental supplies) and total ecological footprint (environmental demands)."),
                           box(title = "What does biocapacity deficit in a country means ?",width = 5,height =220, background = "olive",
                           "If the total ecological footprint of a country is higher than its total biocapacity this country has an ecological deficit so it is considered in biocapacity deficit.
                           The ecological situation of the country isn't durable.",br(),"Moreover a national ecological deficit means that the nation is importing biocapacity through trade, liquidating national ecological assets or emitting carbon dioxide waste into the atmosphere.",
                           br(),br(),"Biocapacity reserve is the opposite. "),
                    
                    
                    column(width=7,box(selectInput(inputId="input_biocap2", NULL, choices = c("By Region","By HDI","By Income class")), status = "success",title = "Choose type of biocapacity",width = NULL)),
                           box(radioButtons(inputId="input_biocap3", NULL, choices=c("Country","Region","HDI","Income class"),selected="Country",inline=T),status = "success",title = "Choose your popup",width = 5),
                    column(width=7,box(leafletOutput("mymap5"),height=500,width=NULL,status="success",title="Map of biocapacity"))
                          
          )),
  
  
  
  tabItem(tabName = "Requierement",
          fluidRow(box(leafletOutput("mymap6"),width=6, height =500,status = "success",title="Earths Required"),
                   box(leafletOutput("mymap7"),width=6, height =500,status = "success",title="Countries Required")))
  
          ))

ui <- dashboardPage(header,sidebar,body,skin="black")


##########################################
# Create Shiny app ----
#shinyApp(ui, server)



