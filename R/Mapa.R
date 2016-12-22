#' La función PintarMapa ubica en el mapa los datos de geolocalización
#' devueltos por la consulta realizada con Shodan
#' PintarMapa(df)

PintarMapa <- function(joinedDF) {

  if (length(joinedDF) > 0) {

    locations <- FreeGeoIP(joinedDF$IP)
    require(maps)
    world <- map("world", fill = TRUE, plot = FALSE) 
    
    # kick out a a widget
    require(leaflet)
    leaflet(data=world) %>% 
      addTiles() %>% 
      addCircleMarkers(locations$longitude, locations$latitude, 
                       color = '#ff0000')
    
    
  }
  
}
