# Carga de CVEs
cveDF <- ParseNVD()

# Carga de Shodan
shodanSF <- ParserShodan()

#Une los dataframes de Shodan y CVE
joinedDF <- UnirDatos()

#Cuenta la cantidad de CVEs encontradas por cada CPE de los hosts de Shodan
totalCVEsByCPE <- ContarTotalCVE_CPE(joinedDF)


totalCVEs <- ContarTotalCVEs(cveDF)



