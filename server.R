
server <- function(input, output) {
  
  #Page d'accueil
  output$accueil <- renderLeaflet({
    leaflet(world) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(color="RdYlGn", #color c'est pour la couleur des frontières
                  weight=0.5, #plus c'est petit plus les traits des frontières sont petites
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity=1),
                  smoothFactor = 0.5,
                  label=labelsaccueil,
                  opacity = 0.5, #opacité des frontières
                  fillOpacity = 0.5, #opacité de la couleur des pays 
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
      addPolygons(color="RdYlGn",#color c'est pour la couleur des frontières
                  weight=0.5, #plus c'est petit plus les traits des frontières sont petites
                  highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE,opacity=1),
                  smoothFactor = 0.5,
                  label=inputdata3(),
                  opacity = 0.5, #opacité des frontières
                  fillOpacity = 0.5, #opacité de la couleur des pays 
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

}
  



  