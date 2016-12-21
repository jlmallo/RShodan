#' La función PintarMapa ubica en el mapa los datos de geolocalización
#' devueltos por la consulta realizada con Shodan
#' PintarMapa(df)

PintarMapa <- function(joinedDF) {

  print(joinedDF$IP)
  if (length(joinedDF) > 0) {

    locations <- ipapi::geolocate(joinedDF$IP)

    world <- maps::map("world", fill = TRUE, plot = FALSE)
    leaflet::leaflet(data = world) %>% leaflet::addTiles() %>% leaflet::addCircleMarkers(locations$lon, locations$lat, color = '#003fff')
  }
}
