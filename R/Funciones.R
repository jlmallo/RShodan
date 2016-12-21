# Instalar devtools:
# devtools::install_github("hrbrmstr/ipapi")
library("devtools")
devtools::install_github("gluc/data.tree", method = "wget", force = T)

use_package("knitr")
use_package("devtools")
use_package("ipapi")
use_package("XML")
use_package("leaflet")
use_package("maps")
use_package("httr")
use_package("jsonlite")
use_package("httr")
use_package("data.tree")
use_package("magrittr")
use_package("dplyr")
use_package("tidyr")
use_package("plyr")
use_package("splitstackshape")

#' Descarga los ficheros necesarios para poder cruzar los datos con Shodan
#' La función DescargarFicheros verifica si la carpeta destino existe
#' Si no existe la crea y si existe descarga el fichero y lo descomprime
#' DescargarFicheros()
DescargarFicheros <- function() {
  dataPath  <-  paste0(getwd(),"/data")
  if (!dir.exists(dataPath))
    dir.create(dataPath, showWarnings = TRUE, recursive = FALSE, mode = "0777")

  xmlUrl2 <- "https://static.nvd.nist.gov/feeds/xml/cve/nvdcve-2.0-Modified.xml.zip"
  download.file(xmlUrl2, destfile = file.path(dataPath,"/nvdcve-2.0-modified.zip"), method = "curl")

  unzip(file.path(dataPath,"/nvdcve-2.0-modified.zip"), exdir = file.path(dataPath))
}

#' Une los dataframes que obtenemos de Shodan y CVE's decargados del NIST
#' La función UnirDatos ejecuta una inner_join de los data
#' frames creados mediante Shodan y el fichero XML
#' UnirDatos(df1, df2)
UnirDatos <- function(shodanDF, cveDF) {
  joinedDF <- dplyr::inner_join(shodanDF, cveDF, by = "CPE.product")
  return(joinedDF)
}

#' La función ContarTotalCVE_CPE nos devuelve el total de CVE's que
#' encontramos en cada host devuelto por Shodan
#' ContarTotalCVE_CPE(<data_frame_devuelto_por_la_función_UnirDatos(df1, df2))
ContarTotalCVE_CPE <- function(joinedDF) {
  total <- count(joinedDF, "CPE")
  return(as.numeric(total))
}

#' La función ContarTotalCVEs nos devuelve el total de vulnerabilidades
#' unicas que encontramos después de sanear los datos en el data frame
#' ContarTotalCVEs(df)
ContarTotalCVEs <- function(cveDF) {
  nvdDF <- cveDF[!duplicated(cveDF[,2]),]
  total <- dplyr::summarise(nvdDF,total_de_vulnerabilidades_unicas = n())
  return(as.numeric(total))
}

#' La función ContarTotalCPE_CVE nos devuelve el total de CPE's por CVE's
#' ContarTotalCPE_CVE(df)

ContarTotalCPE_CVE <- function(joinedDF) {
  total <- count(joinedDF, "CVE")
  return(as.numeric(total))
}

#' La función GraficarTotalCVEs nos devuelve la gráfica creada sobre el data
#' frame cveDF que contiene el total de CPE's por CVSS del CVE
#' GraficarTotalCPEByCVEScore()
GraficarTotalCPEByCVEScore <- function(cveDF) {
  barplot(table(cveDF$cvss), col = heat.colors(12), main = "Total de CPEs por CVSS del CVE", xlab = "Score")
}

#' La función GraficarTotalCPEShodanByCVEScore nos devuelve la gráfica de
#' vulnerabilidades de CPE's de Shodan agrupadas por CVSS
#' GraficarTotalCPEByCVEScore(df)
GraficarTotalCPEShodanByCVEScore <- function(joinedDF) {
  nvdDF <- joinedDF[!duplicated(joinedDF[,2]),]
  barplot(table(nvdDF$cvss), col = heat.colors(12), main = "Total de CPEs encontrados por Shodan agrupados por CVSS")

}

#' La función GraficarTotalCVEScore nos devuelve la gráfica de
#' vulnerabilidades de CVE's por CVSS del NVD
#' GraficarTotalCVEScore(df)
GraficarTotalCVEScore <- function(cveDF) {
  nvdDF <- cveDF[!duplicated(cveDF[,2]),]
  barplot(table(nvdDF$cvss), col = heat.colors(12), main = "Total de CVEs por CVSS")
}

#' La función GraficaTotalCVEsAno nos devuelve la gráfica de vulnerabilidades
#' de CVE's por por año del NVD
#' GraficaTotalCVEsAno(df)
GraficaTotalCVEsAno <- function(cveDF) {
  splittedDF <- tidyr::separate(data = data.frame(CVE = unique(cveDF$CVE)), col = CVE, sep = "-", into = c("xx","Year","zz"))
  barplot(table(splittedDF$Year), col = heat.colors(12), main = "Total CVEs por año")
}
