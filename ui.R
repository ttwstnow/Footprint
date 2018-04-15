dt <- read.table(file = "data.csv", sep = ' ', header = TRUE)
cnames <- names(dt)


#######################################################################################################################
#############################################     Header    ###########################################################
#######################################################################################################################

header <-  dashboardHeader(
  title = tags$span(HTML("<img src='data:image/png;base64,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' , height=50/>")),
  dropdownMenu(messageItem(from = "Github", message = "Click for application code", href = "https://github.com/ttwstnow/Footprint.git"),
                 messageItem(from = "Gobal Footprint Network", message = "Click for more informations about data", icon = icon("home"), href =  "http://data.footprintnetwork.org",  time = "10:00"),
                 messageItem(from = "Kaggle", message = "Previous works on data", icon = icon("rss"), time = "2018-04-01", href =  "https://www.kaggle.com/footprintnetwork/ecological-footprint"))
)

#######################################################################################################################
#############################################     Sidebar    ###########################################################
#######################################################################################################################


sidebar <-dashboardSidebar(width=250,
                           sidebarMenu(menuItem("Welcome",tabName = "welcome",icon=icon("leaf")),
                                       menuItem("Explore Data",tabName = "explore", icon = icon("search"),
                                                menuItem("Maps",tabName = "maps", icon = icon("globe"),
                                                         menuSubItem("Features", tabName = "Features", icon = icon("globe")),
                                                         menuSubItem("Biocapacity", tabName = "Biocapacity", icon = icon("globe")),
                                                         menuSubItem("Requierement", tabName = "Requierement", icon = icon("globe"))),
                                                menuItem("Descriptive statistics",tabName = "stats",icon=icon("binoculars"))),
                                       menuItem("Modelling", tabName = "modeling", icon = icon("line-chart"),
                                                menuSubItem("Linear regression", tabName = "reg1"),
                                                menuSubItem("Logistic regression", tabName = "reg2"),
                                                menuSubItem("Multinomial regression", tabName = "reg3"))
                           ))

########################################################
############################ BODY ######################
########################################################

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
                     box(leafletOutput("mymap7"),width=6, height =500,status = "success",title="Countries Required"))),
    
    
  
    tabItem(tabName = "reg1",
            fluidRow(column(width=12,
                            box(verbatimTextOutput("other_val_show"), height =500, status = "success",title="Linear Regression output")),
                     column(width=8,
                            box(selectInput("modelselect",NULL,choices = c("LM regression"="lin_model")),
                                selectInput("y_variable","Select your independent variable", selected = "Biocapacity.Deficit.or.Reserve", choices =cnames, multiple = FALSE),
                                checkboxGroupInput("x_variables","Select explanatory.ies variable.s",choices =cnames),
                                status = "success",title="Select Inputs")))),
    
    tabItem(tabName = "reg2",
            fluidRow(column(width=12,
                            box(verbatimTextOutput("other_val_show2"), height =500, status = "success",title="Logistic regression output")),
                     column(width=8,
                            box(selectInput("modelselect2","Select the model of your choice",choices = c("Binary model"="logreg")),
                                selectInput("y_variable2","Select your independent variable", selected = "Biocap.Deficit.or.Reserve", choices =cnames, multiple = FALSE),
                                checkboxGroupInput("x_variables2","Select explanatory.ies variable.s",choices =cnames),
                                status = "success",title="Select Inputs")))),
    
    
    tabItem(tabName = "reg3",
            fluidRow(column(width=12,
                            box(verbatimTextOutput("other_val_show3"), height =500, status = "success",title="Multinomial regression output")),
                     column(width=8,
                            box(selectInput("modelselect3","Select the model of your choice",choices = c("Multinomial model"="multinomodel")),
                                selectInput("y_variable3","Select your independent variable", choices =cnames, multiple = FALSE),
                                checkboxGroupInput("x_variables3","Select explanatory.ies variable.s",choices =cnames),
                                status = "success",title="Select Inputs"))))
    
    
  ))



#######################################################################################################################
##############################################       ui      ##########################################################
####################################################################################################################### 

ui <- dashboardPage(header, sidebar, body, skin="black")

