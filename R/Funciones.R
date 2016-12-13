# descarga e instalacion de librerias
install.packages("XML")
install.packages("leaflet")
install.packages("maps")

library(devtools)
# Instalar devtools:
devtools::install_github("hrbrmstr/ipapi")
#devtools::install_github("hrbrmstr/shodan")
devtools::install_github("gluc/data.tree")

#library("shodan")
library("XML")
library("leaflet")
library("maps")
library("httr")
library("jsonlite")
library(httr)
library("data.tree")
library(magrittr)

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

  #data.tree
  repos <- as.Node(data)
  #print(repos, "hostnames", "ip_str")

  #magrittr
  #convert this to a data.frame
  reposdf <- repos %>% ToDataFrameTable(CPE = "cpe",
                                        TITLE="title",
                                        IP="ip_str",
                                        TRASNPORT="transport",
                                        PORT="port",
                                        ORG="org",
                                        COUNTRY="country_name",
                                        CITY="city",
                                        POSTAL_CODE="postal_code",
                                        LONGITUDE="longitude",
                                        LATITUDE="latitude")

  y <- reposdf[!duplicated(reposdf[,3]),]

  y
}
