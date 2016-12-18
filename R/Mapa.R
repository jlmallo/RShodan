#' La función (\emph{PintarMapa}) ubica en el mapa los datos de geolocalización
#' devueltos por la consulta realizada con Shodan
#' @return Plot ubicaciones sobre un mapa
#' @example
#' PintarMapa()

PintarMapa <- function(joinedDF) {


  print(joinedDF$IP)
  if(length(joined)>0){
    #' Obtener localizaciones webcams:
    locations <- geolocate(joinedDF$IP)

    #' Preparar y pintar el mapa:
    world <- map("world", fill = TRUE, plot = FALSE)
    leaflet(data = world) %>% addTiles() %>% addCircleMarkers(locations$lon, locations$lat, color = '#003fff')

  }
}
