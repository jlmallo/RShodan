# descarga e instalacion de librerias
install.packages("XML")
install.packages("leaflet")
install.packages("maps")
install.packages("tidyr")

library(devtools)
# Instalar devtools:
#devtools::install_github("hrbrmstr/ipapi")
#devtools::install_github("gluc/data.tree", method = "curl", force=T)

#library("shodan")
library("XML")
library("leaflet")
library("maps")
library("httr")
library("jsonlite")
library(httr)
library("data.tree")
library(magrittr)
library(dplyr)
library(plyr)

# Variables

dataPath <- file.path(getwd(),"data")

# Funciones


DescargarFicheros <- function() {

  if(!dir.exists(dataPath))
    dir.create(dataPath, showWarnings = TRUE, recursive = FALSE, mode = "0777")

  #xmlUrl1 <- "https://static.nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml"
  #download.file(xmlUrl1, destfile = "./data/CPE_dictionary.xml", method = "wget")

  xmlUrl2 <- "https://static.nvd.nist.gov/feeds/xml/cve/nvdcve-2.0-Modified.xml.zip"
  download.file(xmlUrl2, destfile = file.path(dataPath,"nvdcve-2.0-modified.zip"), method = "wget")

  unzip("./data/nvdcve-2.0-modified.zip", exdir = dataPath) #NO SIRVE!!!!
}


#Une los dataframes de Shodan y CVE
UnirDatos <- function()
{
  joinedDF <- dplyr::inner_join(shodanDF, cveDF, by="CPE.product")
  return(joinedDF)
}

#Contar la cantidad de CVEs encontradas por cada CPE de los hosts de Shodan
ContarTotalCVE_CPE <- function(joinedDF){
  total <- count(joinedDF, "CPE.product")
  return(total)
}

#Contar total de vulnerabilidades unicas
ContarTotalCVEs <- function(nvdDF){
  nvdDF <- nvdDF[!duplicated(nvdDF[,2]),]
  total <- dplyr::summarise(nvdDF,total_de_vulnerabilidades_unicas = n())
  return(total)
}


#Contar la cantidad de CPEs por CVE
ContarTotalCPE_CVE <- function(joinedDF){
  total <- count(joinedDF, "CVE")
  return(total)
}





