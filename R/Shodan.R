#Realiza busque de informacion en Shodan
#' La función ConsultarShodan consulta a Shodan
#' Datos de Shodan
#' ConsultarShodan(<parametros de consulta>)
ConsultarShodan <- function(query){

  facets = NULL
  page = 1
  minify = TRUE
  shodan_base_url <- "https://api.shodan.io"
  shodan_api_key <- "Poner la API aquí"

  facets <- paste(facets, collapse = ",")

  res <- httr::GET(shodan_base_url, path = "shodan/host/search", query = list(query = query, facets = facets, page = page, minify = minify, key = shodan_api_key))

  httr::stop_for_status(res)
  httr::warn_for_status(res, task = NULL)
  httr::message_for_status(res, task = NULL)

  data <- jsonlite::fromJSON(httr::content(res, as = "text"), simplifyDataFrame = FALSE)
  return(data)
}
