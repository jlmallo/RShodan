

#extraer ips webcams:

PintarMapa <- function(ips_list) {
  #obetener localizaciones webcams:
  locations<-geolocate(ip_list)

  #preparar y pintar el mapa:
  world <- map("world", fill = TRUE, plot = FALSE)
  leaflet(data=world) %>% addTiles() %>% addCircleMarkers(locations$lon, locations$lat, color = '#003fff')


}
