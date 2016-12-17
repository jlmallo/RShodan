# descarga e instalacion de librerias
#install.packages("XML")
#install.packages("leaflet")
#install.packages("maps")
#install.packages("tidyr")

# Instalar devtools:
#devtools::install_github("hrbrmstr/ipapi")
#devtools::install_github("gluc/data.tree", method = "curl", force=T)

#library("shodan")
library("devtools")
library("ipapi")
library("XML")
library("leaflet")
library("maps")
library("httr")
library("jsonlite")
library("httr")
library("data.tree")
library("magrittr")
library("dplyr")
library("tidyr")
library("plyr")
library("splitstackshape")

#' Descarga los ficheros necesarios para poder cruzar los datos con Shodan
#' La función (\emph{DescargarFicheros}) verifica si la carpeta destino existe
#' Si no existe la crea y si existe descarga el fichero y lo descomprime
#' @return Carpeta con los ficheros necesarios
#' @example
#' DescargarFicheros()
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
#' ContarTotalCVEs()
ContarTotalCVEs <- function(){
  nvdDF <- joinedDF[!duplicated(joinedDF[,2]),]
  total <- dplyr::summarise(nvdDF,total_de_vulnerabilidades_unicas = n())
  return(total)
}

#' La función (\emph{ContarTotalCPE_CVE}) nos devuelve el total de CPE's por CVE's
#' @return Cantidad de CPE's por CVE
#' @example
#' ContarTotalCPE_CVE()

ContarTotalCPE_CVE <- function(){
  total <- count(joinedDF, "CVE")
  return(total)
}

#' La función (\emph{GraficarTotalCVEs}) nos devuelve la gráfica creada sobre el data frame cveDF
#' que contiene el total de CPE's por CVSS del CVE
#' @return Gráfico de barras
#' @example
#' GraficarTotalCPEByCVEScore()
GraficarTotalCPEByCVEScore <- function()
{
  barplot(table(cveDF$cvss), col = heat.colors(12), main = "Total de CPEs por CVSS del CVE")
}

#' La función (\emph{GraficarTotalCPEShodanByCVEScore}) nos devuelve la gráfica de
#' vulnerabilidades de CPE's de Shodan agrupadas por CVSS
#' @return Gráfico de barras
#' @example
#' GraficarTotalCPEByCVEScore()
GraficarTotalCPEShodanByCVEScore <- function()
{
  nvdDF <- joinedDF[!duplicated(joinedDF[,2]),]
  barplot(table(nvdDF$cvss), col = heat.colors(12), main = "Total de CPEs encontrados por Shodan agrupados por CVSS")

}

#' La función (\emph{GraficarTotalCVEScore}) nos devuelve la gráfica de
#' vulnerabilidades de CVE's por CVSS del NVD
#' @return Gráfico de barras
#' @example
#' GraficarTotalCVEScore()
GraficarTotalCVEScore <- function()
{
  nvdDF <- cveDF[!duplicated(cveDF[,2]),]
  barplot(table(nvdDF$cvss), col = heat.colors(12), main = "Total de CVEs por CVSS")
}

#' La función (\emph{GraficaTotalCVEsAno}) nos devuelve la gráfica de
#' vulnerabilidades de CVE's por por año del NVD
#' @return Gráfico de barras
#' @example
#' GraficaTotalCVEsAno()
GraficaTotalCVEsAno <- function()
{
  splittedDF <- tidyr::separate(data = data.frame(CVE = unique(cveDF$CVE)), col = CVE, sep = "-", into = c("xx","Year","zz"))
  barplot(table(splittedDF$Year), col = heat.colors(12), main = "Total CVEs por año")
}
