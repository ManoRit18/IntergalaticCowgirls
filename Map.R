library(leaflet)
library(shiny)

wa_locations = read.csv("wa_locations.csv")

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Space Exploration in Washington"),
  leafletOutput("mymap"),
  p(),
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({

    m <- leaflet() %>% setView(lng = -120.7401, lat = 47.7511, zoom = 6)
    m %>% addProviderTiles(providers$CartoDB.Positron)
    
    rocketIcons <- icons(
      iconUrl = ifelse(wa_locations$Category == "Business",
        "greenrocket.jpg",
        "redrocket.png"),
      iconWidth = 38, iconHeight = 90,
      iconAnchorX = 22, iconAnchorY = 94,)
    
    leaflet(data = wa_locations) %>% addTiles() %>%
      addMarkers(~-Longitude, ~Latitude, popup = ~as.character(Name), icon=rocketIcons)
  })
}

shinyApp(ui, server)