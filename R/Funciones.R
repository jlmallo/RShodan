# descarga e instalacion de librerias
#install.packages("XML")
#install.packages("leaflet")
#install.packages("maps")

# Instalar devtools:
#devtools::install_github("hrbrmstr/ipapi")
#devtools::install_github("hrbrmstr/shodan")

#library("shodan")
library("XML")
library("leaflet")
library("maps")
library("httr")
library("jsonlite")
library(httr)

# Variables
shodan_base_url  <- "https://api.shodan.io"
shodan_api_key <- "D32FBKHYYqETSf4bIdmurM7xoZA74FnL"

# Funciones

DescargarFicheros <- function() {
  xmlUrl1 <- "https://static.nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml"
  download.file(xmlUrl1, destfile = "./data/CPE_dictionary.xml", method = "wget")
  xmlUrl2 <- "https://static.nvd.nist.gov/feeds/xml/cve/nvdcve-2.0-Modified.xml.zip"
  download.file(xmlUrl2, destfile = "./data/nvdcve-2.0-modified.zip", method = "wget")
  untar("./XML/nvdcve-2.0-modified.zip", exdir = "./data")
}

ConsultarShodan <- function(query=NULL, facets=NULL, page=1, minify=TRUE){


  facets <- paste(facets, collapse = ",")
  res <- GET(shodan_base_url,path = "shodan/host/search",query = list(query = query,
                                                                  facets = facets,
                                                                  page = page,
                                                                  minify = minify,
                                                                  key = shodan_api_key))

  stop_for_status(res)
  warn_for_status(res, task = NULL)

  message_for_status(res, task = NULL)

  data <- fromJSON(content(res, as = "text"))

  test4 <- list('IP'=data$matches$ip)
  test5 <- list('IP'=data$matches$hostnames)

  writers_df <- data.frame(test4, test5)

  writers_df
}

