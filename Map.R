library(leaflet)

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))

m <- leaflet() %>% setView(lng = -120.7401, lat = 47.7511, zoom = 6.4)
m %>% addProviderTiles(providers$CartoDB.Positron)



