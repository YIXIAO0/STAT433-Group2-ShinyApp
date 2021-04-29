massShootings <- readRDS("data/mass_shooting_data_1.rds")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(mapproj)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("leaflet", repos = "http://cran.us.r-project.org")



# User Interface ---
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                ## the max number of mass shooting is an extreme number, most nums are less than 50
                sliderInput("range", "Number of Victims", min(massShootings$Total.Victims), max(massShootings$Total.Victims),
                            value = range(massShootings$Total.Victims), step = 1
                ),
                sliderInput("YearRange", "Year", min(massShootings$Year), max(massShootings$Year),
                            value = range(massShootings$Year), step = 1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                selectInput("target", "Choose the target type", choices = c("All", unique(massShootings$Target))),
                selectInput("cause", "Chooce the cause", choices = c("All", unique(massShootings$Cause))),
                selectInput("open.close", "Choose the location (open/close)", choices = c("All", "Open", "Close")),
                checkboxInput("legend", "Show legend", TRUE),
                mainPanel(
                  plotOutput("targetPlot")
                )
  )
)

server <- function(input, output, session) {
  selectTarget <- reactive({
    massShootings[massShootings$Target == input$target & massShootings$Total.Victims >= input$range[1] & 
                    massShootings$Total.Victims <= input$range[2]
                  & massShootings$Year >= input$YearRange[1] & massShootings$Year <= input$YearRange[2],]
    
  })
  selectCause <- reactive({
    massShootings[massShootings$Total.Victims >= input$range[1] & massShootings$Total.Victims <= input$range[2]
                  & massShootings$Year >= input$YearRange[1] & massShootings$Year <= input$YearRange[2] &
                    massShootings$Cause == input$cause,]
  })
  
  selectOC <- reactive({
    massShootings[massShootings$Total.Victims >= input$range[1] & massShootings$Total.Victims <= input$range[2]
                  & massShootings$Year >= input$YearRange[1] & massShootings$Year <= input$YearRange[2] &
                    massShootings$Open.Close.Location == input$open.close,]
  })
  
  selectTC <- reactive({
    massShootings[massShootings$Total.Victims >= input$range[1] & massShootings$Total.Victims <= input$range[2]
                  & massShootings$Year >= input$YearRange[1] & massShootings$Year <= input$YearRange[2] &
                    massShootings$Cause == input$cause & massShootings$Target == input$target,]
  })
  
  selectTOC <- reactive({
    massShootings[massShootings$Total.Victims >= input$range[1] & massShootings$Total.Victims <= input$range[2]
                  & massShootings$Year >= input$YearRange[1] & massShootings$Year <= input$YearRange[2] &
                    massShootings$Open.Close.Location == input$open.close & massShootings$Target == input$target,]
  })
  
  selectCOC <- reactive({
    massShootings[massShootings$Total.Victims >= input$range[1] & massShootings$Total.Victims <= input$range[2]
                  & massShootings$Year >= input$YearRange[1] & massShootings$Year <= input$YearRange[2] &
                    massShootings$Open.Close.Location == input$open.close & massShootings$Cause == input$cause,]
  })
  
  selectAll <- reactive({
    massShootings[massShootings$Total.Victims >= input$range[1] & massShootings$Total.Victims <= input$range[2]
                  & massShootings$Year >= input$YearRange[1] & massShootings$Year <= input$YearRange[2] &
                    massShootings$Open.Close.Location == input$open.close & massShootings$Cause == input$cause &
                    massShootings$Target == input$target,]
  })
  
  
  # Reactive expression for the data subsetted to what the user selected
  
  filteredData <- reactive({
    massShootings[massShootings$Total.Victims >= input$range[1] & massShootings$Total.Victims <= input$range[2]
                  & massShootings$Year >= input$YearRange[1] & massShootings$Year <= input$YearRange[2],]
  })

  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, massShootings$Total.Victims)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(massShootings) %>% addTiles() %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  

  
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    if (input$target == "All" & input$cause == "All" & input$open.close == "All"){
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        addCircles(radius = ~100*Total.Victims, weight = 1, color = "#777777",
                   fillColor = ~pal(Total.Victims), fillOpacity = 0.7, popup = ~paste(Total.Victims)
        )
    }else if (input$target == "All" & input$cause == "All"){
      leafletProxy("map", data = selectOC()) %>%
        clearShapes() %>%
        addCircles(radius = ~100*Total.Victims, weight = 1, color = "#777777",
                   fillColor = ~pal(Total.Victims), fillOpacity = 0.7, popup = ~paste(Total.Victims)
        )
    }else if (input$target == "All" & input$open.close == "All"){
      leafletProxy("map", data = selectCause()) %>%
        clearShapes() %>%
        addCircles(radius = ~100*Total.Victims, weight = 1, color = "#777777",
                   fillColor = ~pal(Total.Victims), fillOpacity = 0.7, popup = ~paste(Total.Victims)
        )
    }else if (input$cause == "All" & input$open.close == "All"){
      leafletProxy("map", data = selectTarget()) %>%
        clearShapes() %>%
        addCircles(radius = ~100*Total.Victims, weight = 1, color = "#777777",
                   fillColor = ~pal(Total.Victims), fillOpacity = 0.7, popup = ~paste(Total.Victims)
        )
    }else if (input$cause == "All"){
      leafletProxy("map", data = selectTOC()) %>%
        clearShapes() %>%
        addCircles(radius = ~100*Total.Victims, weight = 1, color = "#777777",
                   fillColor = ~pal(Total.Victims), fillOpacity = 0.7, popup = ~paste(Total.Victims)
        )
    }else if (input$target == "All"){
      leafletProxy("map", data = selectCOC()) %>%
        clearShapes() %>%
        addCircles(radius = ~100*Total.Victims, weight = 1, color = "#777777",
                   fillColor = ~pal(Total.Victims), fillOpacity = 0.7, popup = ~paste(Total.Victims)
        )
    }else if (input$open.close == "All"){
      leafletProxy("map", data = selectTC()) %>%
        clearShapes() %>%
        addCircles(radius = ~100*Total.Victims, weight = 1, color = "#777777",
                   fillColor = ~pal(Total.Victims), fillOpacity = 0.7, popup = ~paste(Total.Victims)
        )
    }else{
      leafletProxy("map", data = selectAll()) %>%
        clearShapes() %>%
        addCircles(radius = ~100*Total.Victims, weight = 1, color = "#777777",
                   fillColor = ~pal(Total.Victims), fillOpacity = 0.7, popup = ~paste(Total.Victims)
        )
    }
  })
  
  
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = massShootings)
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Total.Victims
      )
    }
  })
}

shinyApp(ui, server)