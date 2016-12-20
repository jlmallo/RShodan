#' La función (\emph{PintarMapa}) ubica en el mapa los datos de geolocalización
#' devueltos por la consulta realizada con Shodan
#' @return Plot ubicaciones sobre un mapa
#' @example
#' PintarMapa()

PintarMapa <- function(joinedDF) {


  print(joinedDF$IP)
  if (length(joinedDF) > 0) {
    #' Obtener localizaciones webcams:
    locations <- ipapi::geolocate(joinedDF$IP)

    #' Preparar y pintar el mapa:
    world <- maps::map("world", fill = TRUE, plot = FALSE)
    leaftlet::leaflet(data = world) %>% leaftlet::addTiles() %>% leaftlet::addCircleMarkers(locations$lon, locations$lat, color = '#003fff')

  }
}
