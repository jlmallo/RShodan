#Shodan
shodan_base_url   <- "https://api.shodan.io"
shodan_api_key    <- "D32FBKHYYqETSf4bIdmurM7xoZA74FnL"
search_base       <-"apache"  #consulta a shodan

##
#GeolocalizarShodan <- function() {
#  data <- ConsultarShodan()
#  PintarMapa (data[[1]][['ip_str']])
#}
##

#Realiza busque de informacion en Shodan
ConsultarShodan <- function(query=search_base, facets=NULL, page=1, minify=TRUE){

  facets <- paste(facets, collapse = ",")

  #httr
  res <- GET(shodan_base_url,path = "shodan/host/search",query = list(query = query,
                                                                      facets = facets,
                                                                      page = page,
                                                                      minify = minify,
                                                                      key = shodan_api_key))

  #Take action on http error.  --https://www.rdocumentation.org/packages/httr/versions/1.2.1/topics/stop_for_status
  stop_for_status(res)
  warn_for_status(res, task = NULL)
  message_for_status(res, task = NULL)

  #jsonlite
  data <- fromJSON(content(res, as = "text"), simplifyDataFrame = FALSE)
  return(data)
}

