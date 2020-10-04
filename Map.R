library(leaflet)
library(shiny)

wa_locations = read.csv("wa_locations.csv")

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Take Off: Washington"),
  
  sidebarLayout(
    sidebarPanel(
      column(3, 
             checkboxGroupInput(
               inputId = "myGroup",
               label = "Choose:", 
               choiceNames = list(
                 tags$span("Business", style = "color: green;"),
                 tags$span("Academic", style = "color: red;"), 
                 tags$span("Recreational", style = "color: blue;")
               ),
               choiceValues = c("Business", "Academic", "Recreational")
             )
      )
    ),
    mainPanel(
      leafletOutput("mymap"),
      
      tabsetPanel(
                  tabPanel("About", textOutput("about") ),
                  tabPanel("Local Events", textOutput("local_events")),
                  tabPanel("News", textOutput("news")),
                  tabPanel("Women from Washington", htmlOutput("notable1"),
                           htmlOutput("notable2"), htmlOutput("notable3"), htmlOutput("notable4"))
      ),
    ),
    
  ),

  p(),
)

server <- function(input, output, session) {
  
  output$about <- renderText({
    "this is text1"
  })
  
  output$local_events <- renderText({
    "this is text2"
  })
  
  output$news <- renderText({
    tags$a(href="https://www.nasa.gov/image-feature/goddard/2020/noaanasas-suomi-npp-satellite-focuses-on-washington-states-ring-of-fire/", 
           "Satellites Take Images of Washington Wildfires")
  })
  
  output$notable1 <- renderUI({
    HTML(paste("Thora Waters Halstead: Space biologist (WSU '50)",
          "Jennifer Ross-Nazzal: NASA Historian (WSU '04)",
          "Iris Fujiura Bombelyn: Engineer (WSU '83)", sep="<br/>"))
  })
  
  output$notable2 <- renderUI({
    tags$a(href="https://magazine.wsu.edu/web-extra/space-cougs/", 
           "More WSU graduates who have contributed to space exploration")    
  })
  
  output$notable3 <- renderUI({
    HTML(paste("Dottie Metcalf-Lindenburger: Astronaut",
               "Wendy Lawrence: Astronaut", sep="<br/>"))
  })
  
  output$notable4 <- renderUI({
    tags$a(href="https://www.seattletimes.com/seattle-news/5-washington-astronauts-try-to-put-outer-space-into-words-grandeur-that-is-beyond-what-i-can-describe/", 
           "More on astronauts from Washington State")
  })
  
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