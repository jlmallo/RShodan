ParseNVD <- function() {
  doc <- XML::xmlTreeParse("nvdcve-2.0-Modified.xml")
  cve <- XML::xmlRoot(doc)

  for (i in 1:length(cve)) {
    cveItem <- XML::xmlToList(cve[[i]])
    #quitamos el último porque es el valor attr
    lengthCPEs <- length(cveItem$`vulnerable-software-list`)
    cveId <- cveItem$`cve-id`
    cveDescription <- cveItem$summary
    print(cveId)
    if (lengthCPEs > 0) {
      for (j in 1:lengthCPEs) {
        cpe <- cveItem$`vulnerable-software-list`[j]
        row <- c(CPEs = cpe, CVE = cveId, Description = cveDescription)
        if (i == 1 &  j == 1) {
          df <- rbind(data.frame(row))
          #colnames(df) <- c("CPE", "CVE", "Description")
        }
        else {
          df <- rbind(df, data.frame(row))
        }

      }
    }
  }
  df
}