#Shodan
#API Key: D32FBKHYYqETSf4bIdmurM7xoZA74FnL


BuscarShodan <- function() {
  webcams <- shodan_search("SQ-WEBCAM")
  webcams
  PintarMapa (webcams[[1]][['ip_str']])
}
