library(leaflet)
library(shiny)

wa_locations = read.csv("wa_locations.csv")

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Space Exploration in Washington"),
  
  sidebarLayout(
    sidebarPanel(
      column(3, 
             checkboxGroupInput("checkGroup", 
                                h3("Checkbox group"), 
                                choices = c("Business" = 1, 
                                               "Academic" = 2, 
                                               "Recreational" = 3),
                                selected = c(1, 2, 3))),
    ),
    mainPanel(
      leafletOutput("mymap"),
      
      h2("Local Events"),
      
      h2("Recent News"),
      
      h2("Notable Washingtonians"),
      p("Thora Waters Halstead: Space biologist (WSU '50)"),
      p("Jennifer Ross-Nazzal: NASA Historian (WSU '04)"),
      p("Iris Fujiura Bombelyn: Engineer (WSU '83)"),
      tags$a(href="https://magazine.wsu.edu/web-extra/space-cougs/", 
             "More WSU graduates who have contributed to space exploration"),
      p("Dottie Metcalf-Lindenburger: Astronaut"),
      p("Wendy Lawrence: Astronaut"),
      tags$a(href="https://www.seattletimes.com/seattle-news/5-washington-astronauts-try-to-put-outer-space-into-words-grandeur-that-is-beyond-what-i-can-describe/", 
             "5 astronauts from Washington State"),
      h2("About")
    ),
    
  ),

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
                       "greenrocket.png",
                       ifelse(wa_locations$Category == "Academic",
                       "redrocket.png",
                       "bluerocket.png")),
      iconWidth = 38, iconHeight = 90,
      iconAnchorX = 22, iconAnchorY = 40,)
    
    leaflet(data = wa_locations) %>% addTiles() %>%
      addMarkers(~-Longitude, ~Latitude, popup = ~as.character(Name), icon=rocketIcons)
  })
}

shinyApp(ui, server)