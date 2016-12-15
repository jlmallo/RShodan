# descarga e instalacion de librerias
install.packages("XML")
install.packages("leaflet")
install.packages("maps")
install.packages("tidyr")

library("devtools")
# Instalar devtools:
#devtools::install_github("hrbrmstr/ipapi")
#devtools::install_github("gluc/data.tree", method = "curl", force=T)

#library("shodan")
library("XML")
library("leaflet")
library("maps")
library("httr")
library("jsonlite")
library("httr")
library("data.tree")
library("magrittr")
library("dplyr")
library("plyr")
library(splitstackshape)


# Variables

dataPath <- file.path(getwd(),"data")

#' Descarga los ficheros necesarios para poder cruzar los datos con Shodan
#' La función (\emph{DescargarFicheros}) verifica si la carpeta destino existe
#' Si no existe la crea y si existe descarga el fichero y lo descomprime
#' @return Carpeta con los ficheros necesarios
#' @example
#' DescargarFichero()

DescargarFicheros <- function() {

  if (!dir.exists(dataPath))
    dir.create(dataPath, showWarnings = TRUE, recursive = FALSE, mode = "0777")

  xmlUrl2 <- "https://static.nvd.nist.gov/feeds/xml/cve/nvdcve-2.0-Modified.xml.zip"
  download.file(xmlUrl2, destfile = file.path(dataPath,"/nvdcve-2.0-modified.zip"), method = "wget")

  unzip(file.path(dataPath,"/nvdcve-2.0-modified.zip"), exdir = file.path(dataPath))
}


#' Une los dataframes que obtenemos de Shodan y CVE's decargados del NIST
#' La función (\emph{UnirDatos}) ejecuta una (\strong{inner_join}) de los data
#' frames creados mediante Shodan y el fichero XML
#' @return Devuelve un dataframe con los datos unidos mediante una inner_join
#' @example
#' UnirDatos()

UnirDatos <- function()
{
  joinedDF <- dplyr::inner_join(shodanDF, cveDF, by = "CPE.product")
  return(joinedDF)
}

#' La función (\emph{ContarTotalCVE_CPE}) nos devuelve el total de CVE's que
#' encontramos en cada host devuelto por Shodan
#' @return Valor total de CVE per host
#' @example
#' ContarTotalCVE_CPE(<data_frame_devuelto_por_la_función_(\emph{UnirDatos})>)

ContarTotalCVE_CPE <- function(joinedDF){
  total <- count(joinedDF, "CPE.product")
  return(total)
}

#' La función (\emph{ContarTotalCVEs}) nos devuelve el total de vulnerabilidades
#' unicas que encontramos después de sanear los datos
#' @return Cantidad de vulnerabilidades únicas
#' @example
#' ContarTotalCVEs(<data_frame_devuelto_por_el_fichero_(\emph{nvdcve-2.0-modified.xml})>)

ContarTotalCVEs <- function(nvdDF){
  nvdDF <- nvdDF[!duplicated(nvdDF[,2]),]
  total <- dplyr::summarise(nvdDF,total_de_vulnerabilidades_unicas = n())
  return(total)
}

#' La función (\emph{ContarTotalCPE_CVE}) nos devuelve el total de CPE's por CVE's
#' @return Cantidad de CPE's por CVE
#' @example
#' ContarTotalCPE_CVE(<data_frame_devuelto_por_la_función_(\emph{UnirDatos})>)

#Contar la cantidad de CPEs por CVE
ContarTotalCPE_CVE <- function(joinedDF){
  total <- count(joinedDF, "CVE")
  return(total)
}
