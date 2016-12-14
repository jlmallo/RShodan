

#extraer ips webcams:

PintarMapa <- function() {

  data <- ConsultarShodan()

  #obetener localizaciones webcams:
  locations<-geolocate(data[[1]][['ip_str']])

  #preparar y pintar el mapa:
  world <- map("world", fill = TRUE, plot = FALSE)
  leaflet(data=world) %>% addTiles() %>% addCircleMarkers(locations$lon, locations$lat, color = '#003fff')

}
