#Realiza busque de informacion en Shodan
#' La función (\emph{ConsultarShodan}) consulta a Shodan
#' @return Datos de Shodan
#' @example
#' ConsultarShodan(parametros de consulta)
ConsultarShodan <- function(query = NULL, facets = NULL, page = 1, minify = TRUE){

  shodan_base_url <- "https://api.shodan.io"
  shodan_api_key <- "D32FBKHYYqETSf4bIdmurM7xoZA74FnL"
  query <- "apache"  #consulta a shodan

  facets <- paste(facets, collapse = ",")

  #httr
  res <- httr::GET(shodan_base_url, path = "shodan/host/search", query = list(query = query,
                                                                      facets = facets,
                                                                      page = page,
                                                                      minify = minify,
                                                                      key = shodan_api_key))

  #Take action on http error.  --https://www.rdocumentation.org/packages/httr/versions/1.2.1/topics/stop_for_status
  httr::stop_for_status(res)
  httr::warn_for_status(res, task = NULL)
  httr::message_for_status(res, task = NULL)

  #jsonlite
  data <- jsonlite::fromJSON(httr::content(res, as = "text"), simplifyDataFrame = FALSE)
  return(data)
}

