
DescargarFicheros()

# Carga de CVEs
cveDF <- ParseNVD()

# Carga de Shodan
shodanDF <- ParserShodan()

#Une los dataframes de Shodan y CVE
joinedDF <- UnirDatos()

#Cuenta la cantidad de CVEs encontradas por cada CPE de los hosts de Shodan
totalCVEsByCPE <- ContarTotalCVE_CPE(joinedDF)

#Contar total de CVEs unicas
totalCVEs <- ContarTotalCVEs(cveDF)

#Contar total de CPEs que tiene cada CVE
totalCPEsByCVE <- ContarTotalCPE_CVE(cveDF)





