library(leaflet)
library(shiny)

wa_locations = read.csv("wa_locations.csv")

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  
  titlePanel("Take Off: Washington"),
  
  # Checkboxes to filter locations by category (unfinished)
  sidebarLayout(
    sidebarPanel(
      column(3, 
             checkboxGroupInput(
               inputId = "checkGroup",
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
      
      # Displays map
      leafletOutput("mymap"),
      
      # Tabs under map
      tabsetPanel(
                  tabPanel("About", htmlOutput("about"), htmlOutput("about1"), htmlOutput("about2"), htmlOutput("about3"), htmlOutput("about4") ),
                  tabPanel("Local Events", htmlOutput("local_events1"), htmlOutput("local_events2"), htmlOutput("local_events3")),
                  tabPanel("News", htmlOutput("news1"), htmlOutput("news2"), htmlOutput("news3")),
                  tabPanel("Women from Washington", htmlOutput("notable1"),
                           htmlOutput("notable2"), htmlOutput("notable3"), htmlOutput("notable4"))
      ),
    ),
  ),
  p(),
)

server <- function(input, output, session) {
  
  # Text for About tab
  output$about <- renderUI({
    HTML(paste("“Take Off” is an interactive map that allows you to learn more about local 
               places that contribute to the work of space exploration and education. 
               Click on a location to receive more information about it, and use the filters
               on the left.", "We hope this resource is helpful in teaching you about 
               how space technology helps our community and finding new organizations and
               events.", sep="<br/>"))
  })
  
  output$about1 <- renderUI({
    HTML(paste("","Here are ways that working in space technology benefits us here on Earth:", sep="<br/>"))
  })
  
  output$about2 <- renderUI({
    tags$a(href="https://www.nasa.gov/mission_pages/station/research/news/15_ways_iss_benefits_earth", 
           "International Space Station")
  })
  
  output$about3 <- renderUI({
    tags$a(href="https://www.nasa.gov/mission_pages/station/research/benefits/index.html", 
           "International Space Station 2")
  })
  
  output$about4 <- renderUI({
    tags$a(href="https://www.nasa.gov/specials/60counting/tech.html ", 
           "60 Decades of NASA Technology")
  })
  
  # Text for Local Events tab
  output$local_events1 <- renderUI({
    HTML(paste("Constellation Tour: 10/5 at 5 PM",
               "Global Star Party: 10/6 at 6 PM",
               "Virtual Star Party: 10/9 at 6:30 PM",sep="<br/>"))
  })
  
  output$local_events2 <- renderUI({
    tags$a(href="https://nightsky.jpl.nasa.gov/clubs-and-events.cfm", 
           "Find astronomy clubs and other events near you!")
  })
  
  output$local_events3 <- renderUI({
    tags$a(href="https://spotthestation.nasa.gov/sightings/view.cfm?country=United_States&region=Washington&city=Seattle#.X3pkwGhKg2w", 
           "Spot the International Space Station over Washington!")
  })
  
  # Text for News tab
  output$news1 <- renderUI({
    tags$a(href="https://www.nasa.gov/image-feature/goddard/2020/noaanasas-suomi-npp-satellite-focuses-on-washington-states-ring-of-fire/", 
           "Satellites Take Images of Washington Wildfires")
  })
  
  output$news2 <- renderUI({
    tags$a(href="https://www.cnn.com/2020/09/30/business/spacex-starlink-washington-scn-trnd/index.html", 
           "How an internet system from space is helping firefighters in Washington")
  })
  
  output$news3 <- renderUI({
    tags$a(href="https://www.geekwire.com/2020/univ-washington-wins-nasa-grant-create-spacey-contest-artemis-student-challenges/", 
           "University of Washington wins NASA grant to create spacey contest for Artemis Student Challenges")
  })
  
  # Text for Women from Washington tab
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
  
  # Displays correct rocket color based on category
  rocketIcons <- icons(
    iconUrl = ifelse(wa_locations$Category == "Business",
                     "greenrocket.png",
                     ifelse(wa_locations$Category == "Academic",
                            "redrocket.png",
                            "bluerocket.png")),
    iconWidth = 38, iconHeight = 90,
    iconAnchorX = 22, iconAnchorY = 40,)
  
  # Outputs map and rocket markers
  output$mymap <- renderLeaflet({

    m <- leaflet() %>% setView(lng = -120.7401, lat = 47.7511, zoom = 6)
    m %>% addProviderTiles(providers$CartoDB.Positron)
    
    leaflet(data = wa_locations) %>% addTiles() %>%
      addMarkers(~-Longitude, ~Latitude, popup = ~MapLabel, icon=rocketIcons, group = "mygroup")
  })
  
  # Reads checkbox inputs (unfinished)
  mydata_filtered <- reactive(wa_locations[wa_locations$Category %in% input$checkGroup, ])
  
  observeEvent(input$checkGroup, {
    leafletProxy("mymap", data = mydata_filtered()) %>%
      clearGroup ("mygroup") %>% 
      addMarkers(~-Longitude, ~Latitude, 
                 popup = ~MapLabel, 
                 icon=rocketIcons,
                 group = "mygroup")
  })  
}

shinyApp(ui, server)