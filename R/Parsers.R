ParseNVD <- function() {
  doc <- XML::xmlTreeParse(file.path(dataPath,"nvdcve-2.0-modified.xml"))
  cve <- XML::xmlRoot(doc)

  for (i in 1:length(cve)) {
    cveItem <- XML::xmlToList(cve[[i]])
    #quitamos el último porque es el valor attr
    lengthCPEs <- length(cveItem$`vulnerable-software-list`)
    cveId <- cveItem$`cve-id`
    cvss <- cveItem$cvss[1]
    cveDescription <- cveItem$summary
    if (lengthCPEs > 0) {
      for (j in 1:lengthCPEs) {
        cpe <- cveItem$`vulnerable-software-list`[j]
        row <- c(CPE = cpe, CVE = cveId, Descripcion = cveDescription, cvss = cvss)
        if (i == 1 &  j == 1) {
          df <- rbind(data.frame(row))
        }
        else {
          df <- rbind(df, data.frame(row))
        }

      }
    }
  }
  return(df)
}

ParserShodan <- function(){

  #' Invocamos consulta a shodan
  data <- ConsultarShodan()

  #' Data.tree
  repos <- data.tree::as.Node(data)

  #' Paso a data.frame
  reposdf <- repos %>% data.tree::ToDataFrameTable(
                                        IP = "ip_str",
                                        CPE.product = "cpe",
                                        TITLE = "title",
                                        TRASNPORT = "transport",
                                        PORT = "port",
                                        ORG = "org",
                                        COUNTRY = "country_name",
                                        CITY = "city",
                                        POSTAL_CODE = "postal_code",
                                        LONGITUDE = "longitude",
                                        LATITUDE = "latitude")

  #' Elimina duplicados por la columna del IP
  uniques <- reposdf[!duplicated(reposdf[,1]),]

  #' Pone un CPE por linea, ya que existen registros de cpe de wordpress en los apache
  uniques <- splitstackshape::cSplit(uniques, "CPE.product", sep = ",", direction = "long")


  return(uniques)
}
